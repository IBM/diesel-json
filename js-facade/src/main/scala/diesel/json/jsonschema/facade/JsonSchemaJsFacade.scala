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
import diesel.json.{Ast, Json, JsonCompletion}
import diesel.json.jsonschema.JsValue.{fromValue, toValue}
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
  def getJsonParser(schema: Any): DieselParserFacade = {
    val s: Ast.Value            = toValue(schema)
    val schemaVal               = JsonSchema.parse(s, new JsonSchemaParserContext(s))
    val completionConfiguration = JsonCompletion.completionConfiguration(schemaVal)
    val markerPostProcessor     = new JsonMarkerPostProcessor(schemaVal)
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

  @JSExport
  class JsValidationResult(
    val schemaValue: Ast.Value,
    @JSExport val schema: Any,
    val valueValue: Ast.Value,
    @JSExport val value: Any,
    @JSExport val res: JsonSchemaValidationResult
  ) {

    private lazy val errorsByPath: Map[String, js.Array[JsValidationError]] =
      res.getErrors.groupBy(_.path).map { t =>
        val fmtPath  = t._1.format
        val jsErrors = t._2.map(toJsError)
        fmtPath -> jsErrors.toJSArray
      }

    @JSExport
    def getErrors(path: String): js.Array[JsValidationError] = errorsByPath.getOrElse(path, js.Array())

    @JSExport
    def propose(path: String, maxDepth: Int = -1): js.Array[Any] = {
      val parsedPath                = JPath.parsePath(path)
      val proposals: Seq[Ast.Value] = JsonSchema.propose(this.res, valueValue, parsedPath, maxDepth)
      val jsProposals               = proposals.map(v => fromValue(v))
      val res                       = js.Array(jsProposals: _*)
      res
    }

    private lazy val formatsByPath: Map[String, js.Array[String]] =
      res.flatten
        .flatMap { r =>
          r match {
            case sov: SchemaObjectValidation =>
              sov.types.flatMap {
                case sv: TStringValidation =>
                  sv.format
                    .map { case (fmt, _) => Seq((r.path, fmt.name)) }
                    .getOrElse(Seq.empty)
                case _                     =>
                  Seq.empty
              }
            case _                           =>
              Seq.empty
          }
        }
        .groupBy(_._1)
        .map { t =>
          t._1.format -> t._2.map(_._2).toJSArray
        }

    @JSExport
    def getFormats(path: String): js.Array[String] = formatsByPath.getOrElse(path, js.Array())

  }

  @JSExport
  def validate(schema: Any, value: Any): JsValidationResult = {
    val s: Ast.Value = toValue(schema)
    val schemaVal    = JsonSchema.parse(s, new JsonSchemaParserContext(s))
    val v: Ast.Value = toValue(value)
    val res          = schemaVal.validate(v)
    new JsValidationResult(s, schema, v, value, res)
  }

  @JSExportAll
  class JsRenderer(
    val key: String,
    val schemaValue: Any
  )

  @JSExport
  def getRenderers(validationResult: JsValidationResult): js.Map[String, JsRenderer] = {
    validationResult.res
      .flatten
      .flatMap { res =>
        res.renderer.map { r =>
          val schemaValue = fromValue(r.schemaValue)
          res.path.format -> new JsRenderer(r.key, schemaValue)
        }
      }
      .toMap
      .toJSMap
  }

  private def toJsError(e: JsonValidationError): JsValidationError =
    JsValidationError(e.path.format, e.message)

}
