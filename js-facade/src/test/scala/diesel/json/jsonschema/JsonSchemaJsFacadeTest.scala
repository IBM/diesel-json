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

import diesel.facade.ParseRequest
import diesel.json.jsonschema.facade.JsonSchemaJsFacade
import diesel.json.jsonschema.facade.JsonSchemaJsFacade.JsValidationError
import munit.FunSuite

import scala.scalajs.js
import diesel.json.i18n.I18n

class JsonSchemaJsFacadeTest extends FunSuite {

  private def parse(json: String): js.Any = js.JSON.parse(json)

  private def doPropose(schema: js.Any, value: js.Any, path: String): js.Array[Any] = {
    val res = JsonSchemaJsFacade.validate(schema, value)
    res.propose(path)
  }

  test("js validation res get errors") {
    val schema = parse(
      """{
        |   "type": "object",
        |   "properties": {
        |     "foo": {
        |       "type": "string"
        |     }
        |   }
        |}""".stripMargin
    )
    val value  = parse(
      """{
        | "foo": 123
        |}""".stripMargin
    )
    val res    = JsonSchemaJsFacade.validate(schema, value)
    assertEquals(res.getErrors("").toSeq, Seq.empty)
    assertEquals(
      res.getErrors("foo").toSeq,
      Seq(
        JsValidationError("foo", "Invalid type: expected string")
      )
    )
  }

  test("formats") {
    val schema = parse(
      """{
        |   "type": "string",
        |   "format": "date-time"
        |}""".stripMargin
    )
    val value  = parse("\"yalla\"")
    val res    = JsonSchemaJsFacade.validate(schema, value)
    assertEquals(res.getFormats("").toSeq, Seq("date-time"))
  }

  test("no errors") {
    val schema = parse("{}")
    val value  = parse("123")
    val res    = JsonSchemaJsFacade.validate(schema, value)
    assert(res.getErrors("").isEmpty)
  }

  test("nothing validates") {
    val schema = parse("false")
    val value  = parse("123")
    val res    = JsonSchemaJsFacade.validate(schema, value)
    assert(res.getErrors("").size == 1)
    val err    = res.getErrors("").head
    assert(err.path.isEmpty)
    assert(err.message == "Nothing validates")
  }

  test("no dups") {
    val schema = parse("""{
                         |  "type" : "string"
                         |}""".stripMargin)
    val value  = parse("true")
    val res    = JsonSchemaJsFacade.validate(schema, value)
    assert(res.getErrors("").size == 1)
    val err    = res.getErrors("").head
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
    val res    = JsonSchemaJsFacade.validate(schema, value)
    assert(res.getErrors("blah").size == 1)
    val err    = res.getErrors("blah").head
    assert(err.path == "blah")
    assert(err.message == "Invalid type: expected string")
  }

  test("propose") {
    val schema    = parse("""{ "type": "string" }""")
    val value     = parse("123")
    val proposals = doPropose(schema, value, "/")
    assert(proposals.size == 1)
    assert(proposals.head.asInstanceOf[String].isEmpty)
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
    val proposal  = proposals.head.asInstanceOf[js.Dictionary[Any]]
    assert(proposal.size == 1)
    assert(proposal("foo") == null)
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
    val proposal  = proposals.head.asInstanceOf[String]
    assert(proposal == "")
  }

  test("propose enum array") {
    val schema = parse(Examples.EnumArray)
    val value  = parse("null")
    val ps     = doPropose(schema, value, "/")
    ps.foreach(x => println("X " + x))
    assert(ps.length == 2)
    val ps0    = ps(0).asInstanceOf[js.Array[_]]
    ps0.foreach(x => println("ps0 " + x))
    assert(ps(0).asInstanceOf[js.Array[_]].isEmpty)
    assert(ps(1) == null)
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
    val schema       = js.Dynamic.literal(
      "type" -> "string"
    )
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
    val rendererSchema = JsValue.toValue(renderers.get("").get.schemaValue)
    assertEquals(rendererSchema, JsValue.toValue(schema))
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
      JsValue.toValue(renderers.get("foo").get.schemaValue),
      JsValue.toValue(parse("""{
                              |  "type": "string",
                              |  "renderer": "RenderFoo"
                              |}""".stripMargin))
    )
    assertEquals(renderers.get("bar").get.key, "RenderBar")
    assertEquals(
      JsValue.toValue(renderers.get("bar").get.schemaValue),
      JsValue.toValue(parse(
        """{
          |  "type": "string",
          |  "renderer": "RenderBar"
          |}
          |""".stripMargin
      ))
    )
  }

}
