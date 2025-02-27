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

import diesel.{GenericTree, Marker}
import diesel.facade.{DieselParserFacade, MarkerPostProcessor}
import diesel.json.{Ast, Json}
import diesel.json.jsonschema._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}
import diesel.json.i18n.I18n
import diesel.i18n.Lang

import scala.scalajs.js.JSConverters._
import diesel.facade.PredictRequest
import diesel.facade.ParseRequest
import diesel.facade.ParseContext
import diesel.facade.PredictContext

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

  private def predictContextFactory(mpp: MarkerPostProcessor): PredictRequest => PredictContext =
    r => PredictContext(markerPostProcessor = Some(mpp))

  @JSExport
  def getJsonParser(schema: Ast.Value): DieselParserFacade = {
    val schemaVal           = JsonSchema.parse(schema, new JsonSchemaParserContext(schema))
//    val completionConfiguration = JsonCompletion.completionConfiguration(schemaVal)
    val markerPostProcessor = new JsonMarkerPostProcessor(schemaVal)
    new DieselParserFacade(
      Json,
      Some(parseContextFactory(markerPostProcessor)),
      Some(predictContextFactory(markerPostProcessor))
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
    val schema: Ast.Value,
    val value: Ast.Value,
    val res: JsonSchemaValidationResult
  )

  @JSExport
  def getErrors(res: JsValidationResult): js.Array[JsValidationError] = {
    val jsErrs = res.res.getErrors.map(toJsError)
    js.Array(jsErrs: _*)
  }

  @JSExport
  def validate(schema: Ast.Value, value: Ast.Value): JsValidationResult = {
    val schemaVal = JsonSchema.parse(schema, new JsonSchemaParserContext(schema))
    val res       = schemaVal.validate(value)
    new JsValidationResult(schema, value, res)
  }

  @JSExport
  def parseValue(value: String): Ast.Value = {
    Json.parse(value) match {
      case Json.JPRError(err)    => {
        throw new RuntimeException(err);
      }
      case Json.JPRSuccess(_, v) => v
    }
  }

  @JSExport
  def toJsonValue(value: Ast.Value): js.Any = JsonValue.astToJsonValue(value)

  @JSExport
  def stringifyValue(value: Ast.Value): String = value.stringify

  @JSExport
  def propose(validationResult: JsValidationResult, path: String, maxDepth: Int = -1): js.Array[Ast.Value] = {
    val parsedPath                = JPath.parsePath(path)
    val v: Ast.Value              = validationResult.value
    val proposals: Seq[Ast.Value] = JsonSchema.propose(validationResult.res, v, parsedPath, maxDepth)
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
    val schemaValue: Ast.Value
  )

  @JSExport
  def getRenderers(validationResult: JsValidationResult): js.Map[String, JsRenderer] = {
    validationResult.res
      .flatten
      .flatMap { res =>
        res.renderer.map { r =>
          res.path.format -> new JsRenderer(r.key, r.schemaValue)
        }
      }
      .toMap
      .toJSMap
  }

  private def toJsError(e: JsonValidationError): JsValidationError =
    JsValidationError(e.path.format, e.message)

}
