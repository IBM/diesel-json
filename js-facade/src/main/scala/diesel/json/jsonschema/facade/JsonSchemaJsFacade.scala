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

package diesel.json.jsonschema.facade

import diesel.facade._
import diesel.i18n.Lang
import diesel.json.{Json, JsonCompletion}
import diesel.json.i18n.I18n
import diesel.json.jsonschema._
import diesel.{CompletionConfiguration, GenericTree, Marker}

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}

@JSExportTopLevel("JsonSchemaJsFacade")
object JsonSchemaJsFacade {

  class JsonMarkerPostProcessor(private val schema: JsonSchema) extends MarkerPostProcessor {
    override def postProcessMarkers(tree: GenericTree): Seq[Marker] = {
      val existingMarkers = tree.markers
      println("existing", existingMarkers)
      val schemaMarkers   = JsonSchema.postProcessMarkers(schema)(tree)
      println("schema", schemaMarkers)
      existingMarkers ++ schemaMarkers
    }
  }

  private def parseContextFactory(mpp: MarkerPostProcessor): ParseRequest => ParseContext =
    r => ParseContext(markerPostProcessor = Some(mpp))

  private def predictContextFactory(
    mpp: MarkerPostProcessor,
    config: CompletionConfiguration
  ): PredictRequest => PredictContext =
    r => PredictContext(markerPostProcessor = Some(mpp), config = Some(config))

  @JSExport
  def getJsonParser(schema: JsonValue): DieselParserFacade = {
    val schemaVal               = JsonSchema.parse(schema.astValue, new JsonSchemaParserContext(schema.astValue))
    val completionConfiguration = JsonCompletion.completionConfiguration(schemaVal)
    val markerPostProcessor     = new JsonMarkerPostProcessor(schemaVal)
    new DieselParserFacade(
      Json,
      Some(parseContextFactory(markerPostProcessor)),
      Some(predictContextFactory(markerPostProcessor, config = completionConfiguration))
    )
  }

  @JSExportAll
  case class JsValidationError(path: String, message: String)

  // TODO move to validate arg
  @JSExport
  def setLang(lang: String): Unit = {
    I18n.setLang(Lang.fromNavigator(lang).getOrElse(Lang.EN))
  }

  @JSExportAll
  class JsValidationResult(
    val schema: JsonValue,
    val value: JsonValue,
    val res: JsonSchemaValidationResult
  )

  @JSExport
  def getErrors(res: JsValidationResult): js.Array[JsValidationError] = {
    val jsErrs = res.res.getErrors.map(toJsError)
    js.Array(jsErrs: _*)
  }

  @JSExport
  def validate(schema: JsonValue, value: JsonValue): JsValidationResult = {
    val schemaVal = JsonSchema.parse(schema.astValue, new JsonSchemaParserContext(schema.astValue))
    val res       = schemaVal.validate(value.astValue)
    new JsValidationResult(schema, value, res)
  }

  @JSExport
  def parseValue(value: String): JsonValue = {
    Json.parse(value) match {
      case Json.JPRError(err)    => throw new RuntimeException(err)
      case Json.JPRSuccess(_, v) => JsonValue(v.clearPosition)
    }
  }

  @JSExport
  def toJsonValue(value: JsonValue): js.Any = JsonValue.astToJsonValue(value.astValue)

  @JSExport
  def stringifyValue(value: JsonValue): String = value.astValue.stringify

  @JSExport
  def propose(validationResult: JsValidationResult, path: String, maxDepth: Int = -1): js.Array[JsonValue] = {
    val parsedPath                = JPath.parsePath(path)
    val v: JsonValue              = validationResult.value
    val proposals: Seq[JsonValue] =
      JsonSchema.propose(validationResult.res, v.astValue, parsedPath, maxDepth).map(av => JsonValue(av))
    val res                       = js.Array(proposals: _*)
    res
  }

  @JSExport
  def getFormats(validationResult: JsValidationResult, path: String): js.Array[String] = {
    val parsedPath                                = JPath.parsePath(path)
    val filtered: Seq[Schema2020_12.StringFormat] = validationResult.res.flatten
      .filter(_.path == parsedPath)
      .flatMap {
        case sov: SchemaObjectValidation =>
          sov.types.flatMap {
            case sv: TStringValidation =>
              sv.format
                .map { case (fmt, _) => Seq(fmt) }
                .getOrElse(Seq.empty)
            case _                     =>
              Seq.empty
          }
        case _                           =>
          Seq.empty
      }
    val names                                     = filtered.map(_.name)
    js.Array(names: _*)
  }

  @JSExportAll
  class JsRenderer(
    val key: String,
    val schemaValue: JsonValue
  )

  @JSExport
  def getRenderers(validationResult: JsValidationResult): js.Map[String, JsRenderer] = {
    validationResult.res
      .flatten
      .flatMap { res =>
        res.renderer.map { r =>
          res.path.format -> new JsRenderer(r.key, JsonValue(r.schemaValue))
        }
      }
      .toMap
      .toJSMap
  }

  private def toJsError(e: JsonValidationError): JsValidationError =
    JsValidationError(e.path.format, e.message)

}
