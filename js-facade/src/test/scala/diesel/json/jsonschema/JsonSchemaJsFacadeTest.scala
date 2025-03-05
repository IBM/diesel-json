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

import diesel.facade.{ParseRequest, PredictRequest}
import diesel.json.Ast.Builder.num
import diesel.json.Ast.Constants.astNull
import diesel.json.i18n.I18n
import diesel.json.jsonschema.facade.{JsonSchemaJsFacade, JsonValue}
import munit.FunSuite

import scala.scalajs.js

class JsonSchemaJsFacadeTest extends FunSuite {

  private def parse(json: String): JsonValue = JsonSchemaJsFacade.parseValue(json)

  private def doValidate(schema: JsonValue, value: JsonValue): js.Array[JsonSchemaJsFacade.JsValidationError] = {
    JsonSchemaJsFacade.getErrors(
      JsonSchemaJsFacade.validate(schema, value)
    )
  }

  private def doPropose(schema: JsonValue, value: JsonValue, path: String): js.Array[JsonValue] = {
    val res = JsonSchemaJsFacade.validate(schema, value)
    JsonSchemaJsFacade.propose(res, path)
  }

  test("no errors") {
    val schema = parse("{}")
    val value  = parse("123")
    val res    = doValidate(schema, value)
    assert(res.isEmpty)
  }

  test("nothing validates") {
    val schema = parse("false")
    val value  = parse("123")
    val res    = doValidate(schema, value)
    assert(res.size == 1)
    val err    = res.head
    assert(err.path.isEmpty)
    assert(err.message == "Nothing validates")
  }

  test("no dups") {
    val schema = parse("""{
                         |  "type" : "string"
                         |}""".stripMargin)
    val value  = parse("true")
    val res    = doValidate(schema, value)
    assert(res.size == 1)
    val err    = res.head
    assert(err.path.isEmpty)
    assert(err.message == "Invalid type: expected string")
  }

  test("no dups 2") {
    val schema = parse("""{
                         |    "type": "object",
                         |    "properties": {
                         |      "foo": {
                         |        "type": "object",
                         |        "properties": {
                         |          "bar": {
                         |            "type": "number"
                         |          },
                         |          "baz": {
                         |            "type": "string"
                         |          }
                         |        }
                         |      },
                         |      "blah": {
                         |        "type": "string"
                         |      }
                         |    },
                         |    "required": [ "blah" ]
                         |}""".stripMargin)
    val value  = parse("""{
                        |    "foo": {
                        |      "bar": 123,
                        |      "baz": "yalla"
                        |    },
                        |    "blah": true
                        |}""".stripMargin)
    val res    = doValidate(schema, value)
    assert(res.size == 1)
    val err    = res.head
    assert(err.path == "blah")
    assert(err.message == "Invalid type: expected string")
  }

  test("propose") {
    val schema    = parse("""{ "type": "string" }""")
    val value     = parse("123")
    val proposals = doPropose(schema, value, "/")
    assert(proposals.size == 1)
    val s         = proposals.head.astValue.asAstStr.get.v
    assert(s == "")
  }

  test("propose 2") {
    val schema    = parse(
      """{
        |   "type": "object",
        |   "properties": {
        |     "foo": {
        |       "type": "string"
        |     }
        |   }
        |}""".stripMargin
    )
    val value     = parse("{}")
    val proposals = doPropose(schema, value, "/")
    assert(proposals.size == 1)
    val proposal  = proposals.head.astValue.asAstObject.get
    assert(proposal.attributes.length == 1)
    val fooAttr   = proposal.attributes.head
    assertEquals(fooAttr.name.s, "foo")
    assertEquals(fooAttr.value, astNull)
  }

  test("propose 3") {
    val schema    = parse(
      """{
        |   "type": "object",
        |   "properties": {
        |     "foo": {
        |       "type": "string"
        |     }
        |   }
        |}""".stripMargin
    )
    val value     = parse(""" { "foo": "bar" }""")
    val proposals = doPropose(schema, value, "/foo")
    assert(proposals.size == 1)
    val proposal  = proposals.head.astValue.asAstStr.get
    assert(proposal.v == "")
  }

  test("propose enum array") {
    val schema = parse(Examples.EnumArray)
    val value  = parse("null")
    val ps     = doPropose(schema, value, "/")
    assert(ps.length == 2)
    assert(ps(0).astValue.asAstArray.get.elems.isEmpty)
    assert(ps(1).astValue == astNull)
  }

  test("set lang") {
    import I18n._
    assertEquals(I18n.nothingValidates(), "Nothing validates")
    JsonSchemaJsFacade.setLang("de")
    assertEquals(I18n.nothingValidates(), "Keine Prüfdaten")
    JsonSchemaJsFacade.setLang("de-DE")
    assertEquals(I18n.nothingValidates(), "Keine Prüfdaten")
    JsonSchemaJsFacade.setLang("zh")
    assertEquals(I18n.nothingValidates(), "无任何验证")
    JsonSchemaJsFacade.setLang("zh-TW")
    assertEquals(I18n.nothingValidates(), "未驗證任何內容")
  }

  test("parsing should return validation errors") {
    val schema       = parse("""{"type":"string"}""")
    val jsonParser   = JsonSchemaJsFacade.getJsonParser(schema)
    val parseRequest = js.Dynamic.literal("text" -> "true").asInstanceOf[ParseRequest]
    val res          = jsonParser.parse(parseRequest)
    assert(res.success)
    assert(res.error.isEmpty)
    assertEquals(res.styles.length, 1)
    assertEquals(res.markers.length, 1)
    assertEquals(res.markers(0).offset, 0)
    assertEquals(res.markers(0).length, 4)
    assertEquals(res.markers(0).severity, "error")
  }

  test("renderer simple") {
    val schema         = parse(
      """{
        |  "type" : "string",
        |  "renderer": "Yalla"
        |}""".stripMargin
    )
    val value          = parse("\"Yo\"")
    val res            = JsonSchemaJsFacade.validate(schema, value)
    val renderers      = JsonSchemaJsFacade.getRenderers(res)
    assertEquals(renderers.size, 1)
    assertEquals(renderers.get("").get.key, "Yalla")
    val rendererSchema = renderers.get("").get.schemaValue
    assert(rendererSchema == schema)
  }

  test("renderer key nested") {
    val schema    = parse(
      """{
        |  "properties": {
        |   "foo": {
        |     "type": "string",
        |     "renderer": "RenderFoo"
        |   },
        |   "bar": {
        |     "type": "string",
        |     "renderer": "RenderBar"
        |   }
        |  }
        |}""".stripMargin
    )
    val value     = parse(
      """{
        | "foo": "x",
        | "bar": "y"
        |}""".stripMargin
    )
    val res       = JsonSchemaJsFacade.validate(schema, value)
    val renderers = JsonSchemaJsFacade.getRenderers(res)
    assertEquals(renderers.size, 2)
    assertEquals(renderers.get("foo").get.key, "RenderFoo")
    assertEquals(
      renderers.get("foo").get.schemaValue,
      parse("""{
              |  "type": "string",
              |  "renderer": "RenderFoo"
              |}""".stripMargin)
    )
    assertEquals(renderers.get("bar").get.key, "RenderBar")
    assertEquals(
      renderers.get("bar").get.schemaValue,
      parse(
        """{
          |  "type": "string",
          |  "renderer": "RenderBar"
          |}
          |""".stripMargin
      )
    )
  }

  test("parse value") {
    val parsed = JsonSchemaJsFacade.parseValue("123").astValue.clearPosition
    assertEquals(parsed, num("123"))
  }

  test("stringify value") {
    val s = JsonSchemaJsFacade.stringifyValue(JsonValue(num("123")))
    assertEquals(s, "123")
  }

  test("to json value") {
    val v = JsonSchemaJsFacade.toJsonValue(JsonValue(num("123"))).asInstanceOf[JsNumber]
    assertEquals(v.tag, "jv-number")
    assertEquals(v.value, "123")
  }

  test("using facade parser") {
    val schema = JsonSchemaJsFacade.parseValue("""{
                                                 |      "type": "object",
                                                 |      "properties": {
                                                 |        "foo": {
                                                 |          "type": "string"
                                                 |        }
                                                 |      }
                                                 |}""".stripMargin)
    val parser = JsonSchemaJsFacade.getJsonParser(schema)
    val res    = parser.predict(js.Dynamic.literal(
      "text"   -> "{}",
      "offset" -> 1
    ).asInstanceOf[PredictRequest])
    assertEquals(res.error, js.undefined)
    assertEquals(
      res.proposals.map(_.text).toSeq,
      Seq("}", "\"foo\"", "\"\"")
    )
  }

}

@js.native
trait JsNumber extends js.Object {
  val tag: String   = js.native
  val value: String = js.native
}
