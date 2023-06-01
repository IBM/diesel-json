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

import diesel.json.Ast
import diesel.json.Ast.Builder._
import diesel.json.Ast.Constants.{astArray, astNull}
import diesel.json.jsonschema.Schema2020_12._
import diesel.json.i18n.I18n
import diesel.json.i18n.I18n.currentKeyResolver
sealed trait HasPath {
  val path: JPath
}

sealed trait HasChildResults {
  def getChildren: Seq[JsonSchemaValidationResult]
}

sealed trait HasProposals {
  def getProposals(results: Set[JsonSchemaValidationResult] = Set.empty): Seq[Ast.Value]
}

sealed trait JsonSchemaValidationResult extends HasErrors with HasChildResults with HasPath with HasProposals {

  val schema: JsonSchema

  def flatten: Seq[JsonSchemaValidationResult] = {
    val childResults = this.getChildren.flatMap(_.flatten)
    Seq(this) ++ childResults
  }

  def renderer: Option[Renderer]

}

sealed trait JsonValidationError                                                          extends HasPath             {
  def message: String
}
case class InvalidTypeError(path: JPath, expected: Seq[String])                           extends JsonValidationError {
  override def message: String = I18n.invalidType(expected)
}
object InvalidTypeError {
  def apply(path: JPath, expected: String): InvalidTypeError = InvalidTypeError(path, Seq(expected))
}
case class StringMinLengthError(path: JPath, min: Int)                                    extends JsonValidationError {
  override def message: String = I18n.invalidStringLengthMin(min)
}
case class StringMaxLengthError(path: JPath, max: Int)                                    extends JsonValidationError {
  override def message: String = I18n.invalidStringLengthMax(max)
}
case class PatternNotMatchingError(path: JPath, p: String)                                extends JsonValidationError {
  override def message: String = I18n.notMatchingStringPattern(p)
}
case class RequiredPropertyError(path: JPath, names: Seq[String])                         extends JsonValidationError {
  override def message: String = I18n.missingRequiredProperties(names.length)(names)
}
case class PropertyNameError(path: JPath, name: String, errors: Seq[JsonValidationError]) extends JsonValidationError {
  override def message: String = I18n.invalidPropertyName(name, errors.map(_.message))
}
case class InvalidArrayLengthError(path: JPath, expectedLength: Int)                      extends JsonValidationError {
  override def message: String = I18n.invalidArrayLength(expectedLength)
}
case class NothingValidatesError(path: JPath)                                             extends JsonValidationError {
  override def message: String = I18n.nothingValidates()
}
case class ValueNotInEnumError(path: JPath, enumValues: Seq[Ast.Value])                   extends JsonValidationError {
  override def message: String = I18n.valueNotInEnum(enumValues.map(_.toString))
}
case class InvalidConstantError(path: JPath, constantValue: Ast.Value)                    extends JsonValidationError {
  override def message: String = I18n.invalidConstant(constantValue.toString)
}
case class NotFailedError(path: JPath)                                                    extends JsonValidationError {
  override def message: String = I18n.notFailed()
}
case class StringFormatError(path: JPath, expected: StringFormat)                         extends JsonValidationError {
  override def message: String = I18n.invalidStringFormat(expected.name)
}

sealed trait HasErrors {
  val isSuccess: Boolean = getErrors.isEmpty
  def getErrors: Seq[JsonValidationError]
}

sealed trait TypeValidation extends HasErrors with HasChildResults with HasPath with HasProposals {

  val invalidType: Boolean

  def getInvalidTypeErrors(expected: String): Seq[JsonValidationError] =
    if (invalidType) {
      Seq(InvalidTypeError(path, Seq(expected)))
    } else {
      Seq.empty
    }
}

case class TStringValidation(
  path: JPath,
  invalidType: Boolean,
  minLength: Option[(Int, Boolean)] = None,
  maxLength: Option[(Int, Boolean)] = None,
  pattern: Option[(String, Boolean)] = None,
  format: Option[(StringFormat, Boolean)] = None
) extends TypeValidation {

  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = Seq(Ast.Constants.astStr)

  override def getChildren: Seq[JsonSchemaValidationResult] = Seq.empty

  override def getErrors: Seq[JsonValidationError] = {
    val invalidTypeErrors = getInvalidTypeErrors("string")
    val minLengthErrors   = minLength
      .map {
        case (l, b) if !b =>
          Seq(StringMinLengthError(path, l))
        case _            =>
          Seq.empty
      }
      .getOrElse(Seq.empty)

    val maxLengthErrors = maxLength
      .map {
        case (l, b) if !b =>
          Seq(StringMaxLengthError(path, l))
        case _            =>
          Seq.empty
      }
      .getOrElse(Seq.empty)

    val patternErrors = pattern
      .map { case (p, matches) =>
        if (!matches) {
          Seq(PatternNotMatchingError(path, p))
        } else {
          Seq.empty
        }
      }
      .getOrElse(Seq.empty)

    val formatErrors = format
      .map {
        case (sf, matches) if !matches =>
          Seq(StringFormatError(path, sf))
        case _                         =>
          Seq.empty
      }
      .getOrElse(Seq.empty)

    invalidTypeErrors ++
      minLengthErrors ++
      maxLengthErrors ++
      patternErrors ++
      formatErrors
  }
}

case class TNumberValidation(
  path: JPath,
  invalidType: Boolean
) extends TypeValidation {
  override def getChildren: Seq[JsonSchemaValidationResult]                           = Seq.empty
  override def getErrors: Seq[JsonValidationError]                                    = getInvalidTypeErrors("number")
  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = Seq(Ast.Constants.astNumber)
}

case class TIntegerValidation(
  path: JPath,
  invalidType: Boolean
) extends TypeValidation {
  override def getChildren: Seq[JsonSchemaValidationResult]                           = Seq.empty
  override def getErrors: Seq[JsonValidationError]                                    = getInvalidTypeErrors("integer")
  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = Seq(Ast.Constants.astNumber)
}

case class TObjectValidation(
  tObject: TObject,
  path: JPath,
  invalidType: Boolean,
  properties: Map[String, Option[JsonSchemaValidationResult]],
  patternProperties: Map[String, JsonSchemaValidationResult] = Map.empty,
  additionalProperties: Map[String, JsonSchemaValidationResult] = Map.empty,
  required: Seq[(String, Boolean)] = Seq.empty,
  propertyNames: Map[String, JsonSchemaValidationResult] = Map.empty,
  minProperties: Option[(Int, Boolean)] = None,
  maxProperties: Option[(Int, Boolean)] = None
) extends TypeValidation {

  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = {
    val attrs: Seq[Ast.Attribute] = tObject.properties
      .keys
      .map { name => attr(name, astNull) }
      .toSeq
      .sortBy(_.name.s)
    Seq(obj(attrs))
  }

  override def getChildren: Seq[JsonSchemaValidationResult] =
    properties.values.flatten.toSeq ++
      patternProperties.values.toSeq ++
      additionalProperties.values.toSeq ++
      propertyNames.values.toSeq

  override def getErrors: Seq[JsonValidationError] = {
    val invalidTypeErrors          = getInvalidTypeErrors("object")
    val propertiesErrors           = properties
      .values
      .flatten
      .flatMap(_.getErrors)
    val patternPropertiesErrors    = patternProperties
      .values
      .flatMap(_.getErrors)
    val additionalPropertiesErrors = additionalProperties
      .values
      .flatMap(_.getErrors)
    val requiredErrors             = required
      .flatMap {
        case (name, found) if !found =>
          Seq(RequiredPropertyError(path, Seq(name)))
        case _                       =>
          Seq.empty
      }
    val propertyNamesErrors        = propertyNames
      .toSeq
      .flatMap {
        case (propName, validation) =>
          val nestedErrors = validation.getErrors
          if (nestedErrors.isEmpty) {
            Seq.empty
          } else {
            Seq(PropertyNameError(path, propName, validation.getErrors))
          }
      }

    invalidTypeErrors ++
      propertiesErrors ++
      patternPropertiesErrors ++
      additionalPropertiesErrors ++
      requiredErrors ++
      propertyNamesErrors
  }
}

case class EmptyArrayValidationResult(schema: JsonSchema, path: JPath, result: JsonSchemaValidationResult)
    extends JsonSchemaValidationResult {
  override def getErrors: Seq[JsonValidationError] = Seq.empty

  override def getChildren: Seq[JsonSchemaValidationResult] = Seq.empty

  override def renderer: Option[Renderer] = None

  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = result.getProposals(results)
}

case class ItemsValidation(
  invalidArrayLength: Option[Int],
  validations: Seq[JsonSchemaValidationResult],
  emptyArrayValidation: Option[EmptyArrayValidationResult]
)

case class TArrayValidation(
  tArray: TArray,
  path: JPath,
  invalidType: Boolean,
  items: ItemsValidation
) extends TypeValidation {

  override def getChildren: Seq[JsonSchemaValidationResult] = {
    items.validations ++ items.emptyArrayValidation.map(Seq(_)).getOrElse(Seq.empty)
  }

  override def getErrors: Seq[JsonValidationError] = {
    val invalidTypeErrors   = getInvalidTypeErrors("array")
    val invalidLengthErrors =
      items.invalidArrayLength
        .map(ial => Seq(InvalidArrayLengthError(path, ial)))
        .getOrElse(Seq.empty)
    val itemsErrors         = items.validations.flatMap(_.getErrors)

    invalidTypeErrors ++
      invalidLengthErrors ++
      itemsErrors
  }

  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = {
    Seq(astArray)
  }
}

case class TBooleanValidation(path: JPath, invalidType: Boolean) extends TypeValidation {
  override def getChildren: Seq[JsonSchemaValidationResult]                           = Seq.empty
  override def getErrors: Seq[JsonValidationError]                                    = getInvalidTypeErrors("boolean")
  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = Seq(bool(true), bool(false))
}

case class TNullValidation(path: JPath, invalidType: Boolean) extends TypeValidation {
  override def getChildren: Seq[JsonSchemaValidationResult]                           = Seq.empty
  override def getErrors: Seq[JsonValidationError]                                    = getInvalidTypeErrors("null")
  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = Seq(Ast.Constants.astNull)
}

case class SchemaBoolValidation(schema: SchemaBool, path: JPath, v: Boolean) extends JsonSchemaValidationResult {

  override def getChildren: Seq[JsonSchemaValidationResult] = Seq.empty

  override def renderer: Option[Renderer] = None

  override def getErrors: Seq[JsonValidationError] =
    if (v) {
      Seq.empty
    } else {
      Seq(NothingValidatesError(path))
    }

  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] =
    if (v) {
      Ast.Constants.all
    } else {
      Seq.empty
    }
}

case class SchemaObjectValidation(
  schema: SchemaObject,
  path: JPath,
  types: Seq[TypeValidation] = Seq.empty,
  enum1: (Seq[Ast.Value], Boolean) = (Seq.empty, true),
  const: Option[(Ast.Value, Boolean)] = None,
  allOf: Seq[JsonSchemaValidationResult] = Seq.empty,
  anyOf: Seq[JsonSchemaValidationResult] = Seq.empty,
  oneOf: Seq[JsonSchemaValidationResult] = Seq.empty,
  not: Option[JsonSchemaValidationResult] = None,
  ifThenElse: Option[IfThenElseValidation] = None
) extends JsonSchemaValidationResult {

  override def renderer: Option[Renderer] = schema.renderer

  override def getProposals(results: Set[JsonSchemaValidationResult]): Seq[Ast.Value] = {
    val newResults = results + this

    val enumValues    = enum1._1.map(_.clearPosition)
    val constValues   = const.map(_._1.clearPosition).map(Seq(_)).getOrElse(Seq.empty)
    val defaultValue  = schema.default.toSeq.map(_.clearPosition)
    val exampleValues = schema.examples.map(_.elems).getOrElse(Seq.empty).map(_.clearPosition)

    // do not propose types if we have a default value, const or enums already
    val typeValues = {
      if (exampleValues.nonEmpty || defaultValue.nonEmpty || enumValues.nonEmpty || constValues.nonEmpty) {
        Seq.empty
      } else {
        types.flatMap(_.getProposals(newResults))
      }
    }

    defaultValue ++
      exampleValues ++
      typeValues ++
      enumValues ++
      constValues
  }

  override def getChildren: Seq[JsonSchemaValidationResult] = {
    val t = types.flatMap(_.getChildren)
    t ++
      allOf ++
      anyOf ++
      oneOf ++
      not.map(Seq(_)).getOrElse(Seq.empty) ++
      ifThenElse.map(_.getChildren).getOrElse(Seq.empty)
  }

  private def errorsAnyOf(
    validators: Seq[HasErrors]
  ): Seq[JsonValidationError] = {
    val errs = validators.map(_.getErrors)
    if (errs.exists(_.isEmpty)) {
      Seq.empty
    } else {
      errs.flatten
    }
  }

  private def errorsOneOf(
    validators: Seq[HasErrors]
  ): Seq[JsonValidationError] = {
    val errs     = validators.map(_.getErrors)
    val filtered = errs.filter(_.isEmpty)
    if (filtered.size == 1) {
      Seq.empty
    } else {
      errs.flatten
    }
  }

  override def getErrors: Seq[JsonValidationError] = {
    val typesErrors      = errorsAnyOf(types)
    val enumErrors       =
      if (!enum1._2) {
        Seq(ValueNotInEnumError(path, enum1._1))
      } else {
        Seq.empty
      }
    val constErrors      = const match {
      case Some((value, passed)) if !passed =>
        Seq(InvalidConstantError(path, value))
      case _                                =>
        Seq.empty
    }
    val allOfErrors      = allOf.flatMap(_.getErrors)
    val anyOfErrors      = errorsAnyOf(anyOf)
    val oneOfErrors      = errorsOneOf(oneOf)
    val notErrors        = not match {
      case Some(validation) =>
        if (validation.isSuccess) {
          Seq(NotFailedError(path))
        } else {
          Seq.empty
        }
      case None             =>
        Seq.empty
    }
    val ifThenElseErrors = ifThenElse
      .map(_.getErrors)
      .getOrElse(Seq.empty)

    reduceErrors(typesErrors ++
      enumErrors ++
      constErrors ++
      allOfErrors ++
      anyOfErrors ++
      oneOfErrors ++
      notErrors ++
      ifThenElseErrors)

  }

  private def reduceErrors(errors: Seq[JsonValidationError]): Seq[JsonValidationError] = {
    val errsByPath = errors.groupBy(_.path)
    errsByPath.flatMap { case (_, errs) =>
      reduceErrorsAtPath(errs)
    }.toSeq.sortBy(_.path.format)
  }

  private def reduceErrorsAtPath(errors: Seq[JsonValidationError]) = {
    var invalidTypeError: Option[InvalidTypeError]           = None
    var requiredPropertyError: Option[RequiredPropertyError] = None
    val newErrors                                            = errors.flatMap {
      case x: InvalidTypeError      =>
        invalidTypeError = invalidTypeError
          .map { ite =>
            Some(ite.copy(expected = ite.expected ++ x.expected))
          }
          .getOrElse(Some(x))
        None
      case x: RequiredPropertyError =>
        requiredPropertyError = requiredPropertyError
          .map { rpe =>
            Some(rpe.copy(names = rpe.names ++ x.names))
          }
          .getOrElse(Some(x))
        None
      case x @ _                    =>
        Some(x)
    }
    invalidTypeError.map(Seq(_)).getOrElse(Seq.empty) ++
      requiredPropertyError.map(Seq(_)).getOrElse(Seq.empty) ++
      newErrors
  }
}

case class IfThenElseValidation(
  `if`: JsonSchemaValidationResult,
  `then`: Option[JsonSchemaValidationResult],
  `else`: Option[JsonSchemaValidationResult]
) extends HasChildResults {
  def getErrors: Seq[JsonValidationError] =
    getThenElseValidations.flatMap(_.getErrors)

  private def getThenElseValidations: Seq[JsonSchemaValidationResult] = {
    val thenV = `then`.map(x => Seq(x)).getOrElse(Seq.empty)
    val elseV = `else`.map(x => Seq(x)).getOrElse(Seq.empty)
    thenV ++ elseV
  }

  override def getChildren: Seq[JsonSchemaValidationResult] = Seq(`if`) ++ getThenElseValidations
}

trait JsonSchemaValidator {
  def validate(value: Ast.Value, path: JPath = JPath.empty): JsonSchemaValidationResult
}
