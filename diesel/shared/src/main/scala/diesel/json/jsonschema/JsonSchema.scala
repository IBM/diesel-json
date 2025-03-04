/*
 * Copyright 2018 The Diesel Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package diesel.json.jsonschema

import diesel.SimpleMarkerMessage
import diesel.Errors.SemanticError
import diesel.json.Ast.Constants.{astNull, astObject}
import diesel.json.Ast.{Position, Value}
import diesel.json.{Ast, Json}
import diesel.{GenericTree, Marker}

import java.time.{LocalDate, LocalTime, OffsetDateTime, OffsetTime}
import scala.util.Try
import scala.util.matching.Regex

sealed trait JsonSchema extends JsonSchemaValidator

class JsonSchemaParserContext(
  val schemaRoot: Ast.Value,
  val externalResourceResolver: Option[String => Option[String]] = None
) {

  private val rootId = schemaRoot.asAstObject.flatMap(_.attr("$id")).flatMap(_.asString)

  private var schemasById: Map[String, JsonSchema]      = Map.empty
  private var schemasByNode: Map[Ast.Value, JsonSchema] = Map.empty

  def addSchema(id: Option[String], node: Ast.Value, schema: JsonSchema): JsonSchema = {
    schemasByNode = schemasByNode + (node -> schema)
    id.foreach { i => schemasById = schemasById + (i -> schema) }
    schema

  }

  def resolveDynamicRef(ref: String): Option[JsonSchema] = {
    schemasByNode.values.find {
      case so: Schema2020_12.SchemaObject =>
        so.node.attr("$dynamicAnchor").flatMap(_.asString) match {
          case Some(dynamicAnchor) =>
            "#" + dynamicAnchor == ref
          case None                =>
            false
        }
      case _                              =>
        false
    }
  }

  def resolveRef(ref: String): Option[JsonSchema] = {
    if (ref.startsWith("#")) {
      // internal ref
      val relPath   = ref.drop(1)
      val pathInDoc = JPath.parsePath(relPath)
      val node      = pathInDoc.resolve(schemaRoot)
      node match {
        case Some(schemaNode) =>
          schemasByNode.get(schemaNode) match {
            case x @ Some(_) =>
              x
            case None        =>
              Some(JsonSchema.parse(schemaNode, this))
          }

        case None =>
          // no such node ???
          None
      }
    } else if (ref.contains("#")) {
      ???
    } else {
      schemasById.get(ref) match {
        case s @ Some(_) =>
          s
        case None        =>
          rootId.map(_ + "/" + ref) match {
            case Some(externalRef) =>
              val externalResource = externalResourceResolver.flatMap(resolver => resolver.apply(externalRef))
              externalResource.flatMap { externalSchemaText =>
                Json.parse(externalSchemaText) match {
                  case Json.JPRError(_)          =>
                    None
                  case Json.JPRSuccess(_, value) =>
                    Some(JsonSchema.parse(value, new JsonSchemaParserContext(value, externalResourceResolver)))
                }
              }
            case None              =>
              None
          }
      }
    }
  }
}

trait JsonSchemaParser {

  def parse(value: Ast.Value, context: JsonSchemaParserContext): JsonSchema
}

object JsonSchema extends JsonSchemaParser {

  def parse(
    text: String,
    externalResourceResolver: Option[String => Option[String]] = None
  ): Either[String, (Ast.Value, JsonSchema)] = {
    Json.parse(text) match {
      case Json.JPRError(message)       =>
        Left(message)
      case Json.JPRSuccess(tree, value) =>
        val x = parse(value, new JsonSchemaParserContext(value, externalResourceResolver))
        Right((value, x))
    }
  }

  override def parse(value: Ast.Value, context: JsonSchemaParserContext): JsonSchema = {
    // TODO check $schema to know what version we need to parse
    val schemaParser = Schema2020_12
    schemaParser.parse(value, context)
  }

  private def convertValidationErrorsToMarkers(
    jsonRoot: Ast.Value,
    validationErrors: Seq[JsonValidationError]
  ): Seq[Marker] =
    validationErrors.map { ve =>
      val valueAtPath = ve.path.resolve(jsonRoot)
      val pos         = valueAtPath
        .map(_.position)
        .getOrElse(Position.zero)
      Marker(SemanticError, pos.offset, pos.length, SimpleMarkerMessage(ve.message))
    }

  def postProcessMarkers(schema: JsonSchema)(tree: GenericTree): Seq[Marker] = {
    val value        = tree.root.value.asInstanceOf[Ast.Value]
    val schemaErrors = schema.validate(value).getErrors
    JsonSchema.convertValidationErrorsToMarkers(value, schemaErrors)
  }

  def propose(
    validationResult: JsonSchemaValidationResult,
    root: Ast.Value,
    path: JPath,
    maxDepth: Int,
    proposed: Map[Ast.Value, JPath] = Map.empty
  ): Seq[Ast.Value] = {

    val resultsAtPath = validationResult.flatten.filter(_.path == path)
    val proposals     = resultsAtPath.flatMap(_.getProposals()).distinct
    if (maxDepth < 0) {
      proposals
    } else {
      proposals.map {
        case x: Ast.Object =>
          // prevent cycles !
          val alreadyProposedPath = proposed.get(x)

          val alreadyProposed = alreadyProposedPath.exists(_.isParentOf(path))
          if (alreadyProposed) {
            astObject
          } else {

            // it's an object : we create a new top-level
            // value from this one in order to call
            // the validator a second time on all props
            val newRoot             = Value.map(root, path) { xPath =>
              x
            }
            val newValidationResult = validationResult.schema.validate(newRoot)
            val attrProposals       = x.attributes
              .flatMap { attr =>
                // propose for this attr
                val newProposed   = proposed + (x -> path)
                val attrProposals = propose(
                  newValidationResult,
                  newRoot,
                  path.append(attr.name.s),
                  maxDepth - 1,
                  newProposed
                )
                // replace initially proposed value
                attrProposals.headOption
                  .map { attrProposal =>
                    attr.copy(value = attrProposal)
                  }
              }
            x.copy(attributes =
              attrProposals
            )
          }
        case x @ _         =>
          x
      }
    }
  }

}

object Schema2020_12 extends JsonSchemaParser {

  private def log(x: Any): Unit = {} // println(x)

  sealed trait StringFormat {
    val name: String
    def matches(str: String): Boolean
  }

  object StringFormat {
    def parse(fmt: String): Option[StringFormat] = fmt match {
      case "date"      =>
        Some(FDate)
      case "date-time" =>
        Some(FDateTime)
      case "time"      =>
        Some(FTime)
      case _           =>
        None
    }
  }

  sealed trait DatesAndTimes extends StringFormat

  case object FDateTime extends DatesAndTimes {
    override val name: String = "date-time"

    override def matches(str: String): Boolean = {
      try {
        OffsetDateTime.parse(str)
        true
      } catch {
        case e: Exception =>
          false
      }
    }
  }

  case object FTime extends DatesAndTimes {
    override val name: String = "time"

    override def matches(str: String): Boolean = {
      Try(OffsetTime.parse(str))
        .orElse(Try(LocalTime.parse(str)))
        .map(_ => true)
        .getOrElse(false)
    }
  }

  case object FDate extends DatesAndTimes {
    override val name: String = "date"

    override def matches(str: String): Boolean = {
      try {
        LocalDate.parse(str)
        true
      } catch {
        case e: Exception =>
          false
      }
    }
  }

//  sealed trait EmailAddresses extends StringFormat
//  case object Email           extends EmailAddresses
//  case object IdnEmail        extends EmailAddresses

  // TODO more built-in formats
  // https://json-schema.org/understanding-json-schema/reference/string.html#id8

  sealed trait Type {
    def validate(value: Ast.Value, path: JPath): TypeValidation
  }

  case class TString(
    node: Ast.Object,
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    pattern: Option[String] = None,
    format: Option[StringFormat] = None
  ) extends Type {

    def validate(value: Ast.Value, path: JPath): TStringValidation = {
      log("[TString.validate] : " + path + " - " + value + ", schemaNode=" + node.stringify)
      value match {
        case s: Ast.Str =>
          val minLengthErrors = minLength
            .map { minL =>
              (minL, s.v.length >= minL)
            }

          val maxLengthErrors = maxLength
            .map { maxL =>
              (maxL, s.v.length <= maxL)
            }

          val patternErrors = pattern
            .map { p =>
              val str       =
                if (s.v.startsWith("\"") && s.v.endsWith("\"")) {
                  s.v.drop(1).dropRight(1)
                } else {
                  s.v
                }
              val re: Regex = p.r
              val pattern   = re.pattern
              val matcher   = pattern.matcher(str)
              val matches   = matcher.matches()
              (p, matches)
            }

          val formatErrors = format
            .map { sf =>
              (sf, sf.matches(s.v))
            }

          TStringValidation(
            path,
            invalidType = false,
            minLength = minLengthErrors,
            maxLength = maxLengthErrors,
            pattern = patternErrors,
            format = formatErrors
          )

        case _ =>
          TStringValidation(path, invalidType = true)
      }
    }
  }

  case class TNumber(
    node: Ast.Object,
    multipleOf: Option[Double] = None,
    minimum: Option[Double] = None,
    exclusiveMinimum: Option[Double] = None,
    maximum: Option[Double] = None,
    exclusiveMaximum: Option[Double] = None
  ) extends Type {

    def validate(value: Ast.Value, path: JPath): TNumberValidation = {
      log("[TNumber.validate] : " + path + " - " + value + ", schemaNode=" + node.stringify)
      TNumberValidation(path, invalidType = !value.isInstanceOf[Ast.Number])
    }
  }

  case class TInteger(
    node: Ast.Object,
    multipleOf: Option[Int] = None,
    minimum: Option[Int] = None,
    exclusiveMinimum: Option[Int] = None,
    maximum: Option[Int] = None,
    exclusiveMaximum: Option[Int] = None,
    format: Option[String] = None
  ) extends Type {

    def validate(value: Ast.Value, path: JPath): TIntegerValidation = {
      log("[TInteger.validate] : " + path + " - " + value + ", schemaNode=" + node.stringify)
      // TODO validate it's an int !
      TIntegerValidation(path, invalidType = !value.isInstanceOf[Ast.Number])
    }
  }

  sealed trait Dependencies
  case class PropertyDependencies(v: Map[String, Seq[String]] = Map.empty) extends Dependencies
  case class SchemaDependencies(v: Map[String, JsonSchema] = Map.empty)    extends Dependencies

  case class TObject(
    node: Ast.Object,
    properties: Map[String, JsonSchema] = Map.empty,
    patternProperties: Map[String, JsonSchema] = Map.empty,
    additionalProperties: Option[JsonSchema] = None,
    required: Seq[String] = Seq.empty,
    propertyNames: Option[JsonSchema] = None,
    minProperties: Option[Int] = None,
    maxProperties: Option[Int] = None,
    dependencies: Option[Dependencies] = None
  ) extends Type {

    def validate(value: Ast.Value, path: JPath): TObjectValidation = {
      log("[TObject.validate] : " + path + " - " + value + ", schemaNode=" + node.stringify)

      val props       = properties.map { case (propName, propSchema) =>
        (propName, value.asAstObject.flatMap(_.attr(propName)), propSchema)
      }
      val propsErrors = props.map { case (str, maybeValue, schema) =>
        (str -> maybeValue.map(v => schema.validate(v, path.append(str))))
      }.toMap

      val patternPropsErrors: Map[String, JsonSchemaValidationResult] = value
        .asAstObject
        .map { o =>
          patternProperties
            .flatMap { case (pattern, schema) =>
              val regex         = pattern.r
              val filteredAttrs = o.attributes.filter { attr =>
                regex.findAllIn(attr.name.s).hasNext
              }
              filteredAttrs.map { attr =>
                val attrName = attr.name.s
                (attrName -> schema.validate(attr.value, path.append(attrName)))
              }
            }
        }
        .getOrElse(Map.empty)

      val additionalPropsErrors: Map[String, JsonSchemaValidationResult] = value
        .asAstObject
        .map { o =>
          additionalProperties
            .map { schema =>
              val definedProperies = properties.keys.toSet
              val additionalProps  = o.attributes.filter(attr => !definedProperies.contains(attr.name.s))
              additionalProps.map { attr =>
                val attrName = attr.name.s
                (attrName -> schema.validate(attr.value, path.append(attrName)))
              }.toMap
            }
            .getOrElse(Map.empty)
        }
        .getOrElse(Map.empty)

      val requiredErrors = value
        .asAstObject
        .map { o =>
          required.map { requiredPropName =>
            (requiredPropName, o.attr(requiredPropName).isDefined)
          }
        }
        .getOrElse(Seq.empty)

      // property names
      val propertyNameErrors: Map[String, JsonSchemaValidationResult] = value
        .asAstObject
        .map { o =>
          propertyNames
            .map { pns =>
              o.attributes.map { attr =>
                // we need to create a "fake" value here for the attr name...
                val propertyName   =
                  if (attr.name.s.startsWith("\"") && attr.name.s.endsWith("\"")) {
                    attr.name.s.drop(1).dropRight(1)
                  } else {
                    attr.name.s
                  }
                val nameAsValue    = Ast.Str(attr.name.position, propertyName)
                val nameValidation = pns.validate(nameAsValue, path)
                (propertyName -> nameValidation)
              }.toMap
            }
            .getOrElse(Map.empty)
        }
        .getOrElse(Map.empty)

      TObjectValidation(
        this,
        path,
        !value.isInstanceOf[Ast.Object],
        propsErrors,
        patternProperties = patternPropsErrors,
        additionalProperties = additionalPropsErrors,
        required = requiredErrors,
        propertyNames = propertyNameErrors
      )
    }
  }

  sealed trait Items
  case class ListValidation(schema: JsonSchema)        extends Items
  case class TupleValidation(schemas: Seq[JsonSchema]) extends Items

  case class TArray(
    node: Ast.Object,
    items: Option[Items] = None,
    additionalItems: Option[JsonSchema] = None,
    contains: Option[JsonSchema] = None,
    minItems: Option[Int] = None,
    maxItems: Option[Int] = None,
    uniqueItems: Boolean = false
  ) extends Type {

    def validate(value: Ast.Value, path: JPath): TArrayValidation = {
      log("[TArray.validate] : " + path + " - " + value + ", schemaNode=" + node.stringify)
      value match {
        case a: Ast.Array =>
          val itemsValidation: ItemsValidation = items
            .map {
              case ListValidation(schema)   =>
                val emptyArrayResult =
                  if (a.elems.isEmpty) {
                    // validate a fake value !
                    val newPath = path.append(0)
                    val r       = schema.validate(astNull, newPath)
                    Some(EmptyArrayValidationResult(schema, newPath, r))
                  } else {
                    None
                  }
                ItemsValidation(
                  None,
                  a.elems
                    .zipWithIndex
                    .map { case (elem, index) =>
                      schema.validate(elem, path.append(index))
                    },
                  emptyArrayResult
                )
              case TupleValidation(schemas) =>
                if (a.elems.size == schemas.size) {
                  val validations = a.elems
                    .zipWithIndex
                    .flatMap {
                      case (elem, index) =>
                        schemas.map(_.validate(elem, path.append(index)))
                    }
                  ItemsValidation(None, validations, None)
                } else {
                  ItemsValidation(
                    Some(schemas.size),
                    Seq.empty,
                    None
                  )
                }
            }
            .getOrElse(ItemsValidation(None, Seq.empty, None))

          TArrayValidation(
            this,
            path,
            invalidType = false,
            items = itemsValidation
          )
        case _            =>
          TArrayValidation(this, path, invalidType = true, items = ItemsValidation(None, Seq.empty, None))
      }
    }
  }

  case object TBoolean extends Type {
    def validate(value: Ast.Value, path: JPath): TBooleanValidation = {
      log("[TBoolean.validate] : " + path + " - " + value)
      TBooleanValidation(path, !value.isInstanceOf[Ast.Bool])
    }
  }

  case object TNull extends Type {
    def validate(value: Ast.Value, path: JPath): TNullValidation = {
      log("[TNull.validate] : " + path + " - " + value)
      TNullValidation(path, !value.isInstanceOf[Ast.Null])
    }
  }

  case class SchemaBool(v: Boolean) extends JsonSchema {
    def validate(value: Ast.Value, path: JPath): JsonSchemaValidationResult = {
      SchemaBoolValidation(this, path, v)
    }
  }

  case class IfThenElse(
    `if`: JsonSchema,
    `then`: Option[JsonSchema],
    `else`: Option[JsonSchema]
  ) {
    def validate(value: Ast.Value, path: JPath): IfThenElseValidation = {
      val ifErrs = `if`.validate(value, path)
      val ifTrue = ifErrs.isSuccess
      IfThenElseValidation(
        `if` = ifErrs,
        `then` =
          if (ifTrue) {
            `then`.map(_.validate(value, path))
          } else {
            None
          },
        `else` =
          if (!ifTrue) {
            `else`.map(_.validate(value, path))
          } else {
            None
          }
      )
    }
  }

  case class Renderer(
    key: String,
    schemaValue: Ast.Object
  )

  case class SchemaObject(
    node: Ast.Object,
    title: Option[String] = None,
    description: Option[String] = None,
    id: Option[String] = None,
    types: Seq[Type] = Seq.empty,
    enum1: Seq[Ast.Value] = Seq.empty,
    const: Option[Ast.Value] = None,
    allOf: Seq[JsonSchema] = Seq.empty,
    anyOf: Seq[JsonSchema] = Seq.empty,
    oneOf: Seq[JsonSchema] = Seq.empty,
    not: Option[JsonSchema] = None,
    ifThenElse: Option[IfThenElse] = None,
    default: Option[Ast.Value] = None,
    examples: Option[Ast.Array] = None,
    renderer: Option[Renderer] = None
  ) extends JsonSchema {
    val schema = "http://json-schema.org/draft/2019-09/schema#"

    override def validate(value: Ast.Value, path: JPath): JsonSchemaValidationResult = {
      log("[SchemaObject.validate] : " + path + " - " + value + ", schemaNode=" + node.stringify)
      SchemaObjectValidation(
        this,
        path,
        types =
          types.map(_.validate(value, path)),
        enum1 =
          if (enum1.isEmpty) {
            (Seq.empty, true)
          } else {
            enum1.find(_.equalsIgnorePos(value)) match {
              case Some(_) =>
                (enum1, true)
              case None    =>
                (enum1, false)
            }
          },
        const =
          const.map { v => (v, v.equalsIgnorePos(value)) },
        allOf =
          allOf.map(_.validate(value, path)),
        anyOf =
          anyOf.map(_.validate(value, path)),
        oneOf =
          oneOf.map(_.validate(value, path)),
        not =
          not.map { schema =>
            schema.validate(value, path)
          },
        ifThenElse =
          ifThenElse.map(_.validate(value, path))
      )
    }
  }

  case class SchemaRef(ref: String, context: JsonSchemaParserContext) extends JsonSchema {
    override def validate(value: Ast.Value, path: JPath): JsonSchemaValidationResult = {
      val targetSchemaOpt = context.resolveRef(ref)
      // TODO
      if (targetSchemaOpt.isEmpty) {
        throw new RuntimeException("Schema not found for ref " + ref)
      }
      targetSchemaOpt.get.validate(value, path)
    }
  }

  case class SchemaDynRef(ref: String, context: JsonSchemaParserContext) extends JsonSchema {
    override def validate(value: Ast.Value, path: JPath): JsonSchemaValidationResult = {
      val targetSchemaOpt = context.resolveDynamicRef(ref)
      // TODO
      if (targetSchemaOpt.isEmpty) {
        throw new RuntimeException("Schema not found for ref " + ref)
      }
      targetSchemaOpt.get.validate(value, path)
    }
  }

  override def parse(value: Ast.Value, context: JsonSchemaParserContext): JsonSchema = value match {
    case v: Ast.Object =>
      parseObject(v, context)
    case v: Ast.Bool   =>
      SchemaBool(v.v)
    case _             =>
      SchemaBool(true)
  }

  private def parseTypeUnspecified(obj: Ast.Object, context: JsonSchemaParserContext): Seq[Type] = {
    // we need to look into obj for "known" props in order to decide what to do...
    if (
      obj.attr("properties").isDefined ||
      obj.attr("additionalProperties").isDefined ||
      obj.attr("required").isDefined
    ) {
      Seq(parseType("object", obj, context))
    } else if (obj.attr("pattern").flatMap(_.asString).isDefined) {
      Seq(parseType("string", obj, context))
    } else {
      Seq.empty
    }
  }

  private def parseObject(obj: Ast.Object, context: JsonSchemaParserContext): JsonSchema =
    // check for $ref
    obj.attr("$ref").flatMap(_.asString)
      .map(s => SchemaRef(s, context))
      .getOrElse {
        obj.attr("$dynamicRef").flatMap(_.asString) match {
          case Some(dynamicRef) =>
            SchemaDynRef(dynamicRef, context)
          case None             =>
            val types = obj.attr("type")
              .map {
                case a: Ast.Array =>
                  // multiple types defined
                  a.elems.flatMap {
                    case s: Ast.Str =>
                      Some(parseType(s.v, obj, context))
                    case _          =>
                      println("Unsupported type in array")
                      None
                  }
                case s: Ast.Str   =>
                  // single type defined
                  Seq(parseType(s.v, obj, context))
                case _            =>
                  Seq.empty
              }
              .getOrElse(parseTypeUnspecified(obj, context))

            val renderer = obj
              .attr("renderer")
              .flatMap {
                case Ast.Object(_, attributes) =>
                  val key = attributes.find(_.name.s == "key").flatMap(_.value.asString);
                  key.map(k => Renderer(k, obj))
                case Ast.Str(_, v)             =>
                  Some(Renderer(v, obj))
                case x @ _                     =>
                  println("Unsupported renderer " + x)
                  None
              }

            val id = obj.attr("$id").flatMap(_.asString)
            context.addSchema(
              id,
              obj,
              SchemaObject(
                obj,
                id = id,
                types = types,
                enum1 = parseEnum(obj),
                allOf = parseSchemas(obj, "allOf", context),
                anyOf = parseSchemas(obj, "anyOf", context),
                oneOf = parseSchemas(obj, "oneOf", context),
                ifThenElse = parseIfThenElse(obj, context),
                const = obj.attr("const"),
                default = obj.attr("default"),
                examples = obj.attr("examples").flatMap(_.asAstArray),
                renderer = renderer
              )
            )
        }
      }

  private def parseIfThenElse(value: Ast.Object, context: JsonSchemaParserContext): Option[IfThenElse] = {
    value
      .attr("if")
      .flatMap { ifValue =>
        Some(
          IfThenElse(
            `if` = JsonSchema.parse(ifValue, context: JsonSchemaParserContext),
            `then` = value.attr("then").map(JsonSchema.parse(_, context)),
            `else` = value.attr("else").map(JsonSchema.parse(_, context))
          )
        )
      }
  }

  private def parseSchemas(values: Seq[Ast.Value], context: JsonSchemaParserContext): Seq[JsonSchema] =
    values.map(v => JsonSchema.parse(v, context))

  private def parseSchemas(a: Ast.Array, context: JsonSchemaParserContext): Seq[JsonSchema] =
    parseSchemas(a.elems, context)

  private def parseSchemas(o: Ast.Object, arrayPropName: String, context: JsonSchemaParserContext): Seq[JsonSchema] =
    o.attr(arrayPropName)
      .flatMap(_.asAstArray)
      .map(parseSchemas(_, context))
      .getOrElse(Seq.empty)

  private def parseEnum(obj: Ast.Object): Seq[Ast.Value] =
    obj.attr("enum") match {
      case Some(a: Ast.Array) =>
        a.elems
      case _                  =>
        Seq.empty
    }

  private def parseType(name: String, obj: Ast.Object, context: JsonSchemaParserContext): Type = name match {
    case "null"    =>
      TNull
    case "integer" =>
      parseIntegerType(obj)
    case "number"  =>
      parseNumberType(obj)
    case "string"  =>
      parseStringType(obj)
    case "array"   =>
      parseArrayType(obj, context)
    case "object"  =>
      parseObjectType(obj, context)
    case "boolean" =>
      TBoolean
    case _         =>
      TObject(obj)
  }

  private def parseObjectType(obj: Ast.Object, context: JsonSchemaParserContext): Type = {
    TObject(
      node = obj,
      properties =
        parseTObjectProperties(obj, context),
      required =
        obj.attr("required")
          .flatMap(_.asAstArray)
          .map(_.elems)
          .getOrElse(Seq.empty)
          .flatMap {
            case s: Ast.Str =>
              Some(s.v)
            case _          =>
              None
          },
      additionalProperties =
        obj.attr("additionalProperties")
          .map(v => JsonSchema.parse(v, context)),
      patternProperties =
        obj.attr("patternProperties")
          .flatMap(_.asAstObject)
          .map { patternPropsObj =>
            patternPropsObj.attributes
              .map { attr =>
                val pattern = attr.name.s
                val value   = attr.value
                (pattern, JsonSchema.parse(value, context))
              }
              .toMap
          }
          .getOrElse(Map.empty),
      propertyNames =
        obj.attr("propertyNames")
          .flatMap(_.asAstObject)
          .map(JsonSchema.parse(_, context))
    )
  }

  private def parseTObjectProperties(obj: Ast.Object, context: JsonSchemaParserContext): Map[String, JsonSchema] =
    obj.attr("properties") match {
      case Some(propsObj: Ast.Object) =>
        propsObj.attributes
          .map(attr => (attr.name.s, JsonSchema.parse(attr.value, context)))
          .toMap
      case _                          =>
        Map.empty
    }

  private def parseIntegerType(value: Ast.Object) =
    TInteger(
      node = value,
      format =
        value.attr("format").flatMap(_.asString)
    )

  private def parseNumberType(value: Ast.Object) =
    TNumber(node =
      value
    )

  private def parseStringType(value: Ast.Object) =
    TString(
      node = value,
      minLength = value.attr("minLength").flatMap(_.asInt),
      maxLength = value.attr("maxLength").flatMap(_.asInt),
      pattern = value.attr("pattern").flatMap(_.asString),
      format = value.attr("format").flatMap(_.asString).flatMap(StringFormat.parse)
    )

  private def parseArrayType(value: Ast.Object, context: JsonSchemaParserContext) = {
    TArray(
      node = value,
      items =
        value.attr("items") flatMap {
          case o: Ast.Object =>
            Some(ListValidation(JsonSchema.parse(o, context)))
          case a: Ast.Array  =>
            val schemas = a.elems.map(JsonSchema.parse(_, context))
            Some(TupleValidation(schemas = schemas))
          case _             =>
            None
        }
    )
  }

}
