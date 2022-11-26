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

import diesel.json.jsonschema.facade.JsonSchemaJsFacade
import munit.FunSuite

import scala.scalajs.js
import diesel.json.i18n.I18n

class JsonSchemaJsFacadeTest extends FunSuite {

  private def parse(json: String): js.Any = js.JSON.parse(json)

  private def doValidate(schema: js.Any, value: js.Any): js.Array[JsonSchemaJsFacade.JsValidationError] = {
    JsonSchemaJsFacade.getErrors(
      JsonSchemaJsFacade.validate(schema, value)
    )
  }

  private def doPropose(schema: js.Any, value: js.Any, path: String): js.Array[Any] = {
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

}
