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

import diesel.json.{Ast, Json}
import munit.Assertions.fail

object Util {

  def parseSchema(text: String, externalResourceResolver: Option[String => Option[String]] = None): JsonSchema =
    Json.parse(text) match {
      case Json.JPRError(message)    =>
        fail(s"Unable to parse schema : $message")
      case Json.JPRSuccess(_, value) =>
        parseSchemaValue(value, externalResourceResolver)
    }

  def parseSchemaValue(
    value: Ast.Value,
    externalResourceResolver: Option[String => Option[String]] = None
  ): JsonSchema =
    JsonSchema.parse(value, new JsonSchemaParserContext(value, externalResourceResolver))

  def parseJson(text: String): Ast.Value =
    Json.parse(text) match {
      case Json.JPRError(message)       =>
        fail(message)
      case Json.JPRSuccess(tree, value) =>
        value
    }

  def getErrors(schema: JsonSchema)(json: String): Seq[JsonValidationError] =
    schema.validate(parseJson(json)).getErrors

  def assertErrors(schema: JsonSchema)(json: String)(f: (Ast.Value, Seq[JsonValidationError]) => Unit): Unit = {
    val o    = parseJson(json)
    val errs = schema.validate(o).getErrors
    f(o, errs)
  }

  def assertNoErrors(schema: JsonSchema)(json: String): Unit =
    assertErrors(schema)(json)((_, errs) => errs.isEmpty)

}
