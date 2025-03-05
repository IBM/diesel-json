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
import diesel.json.jsonschema.JPath.Implicits._
import diesel.json.jsonschema.Util.{getErrors, parseSchema}
import munit.FunSuite

class JsonSchemaValidationTest extends FunSuite {

  private def assertNoErrorsEmptyObj(json: String) =
    assert(getErrors(parseSchema("{}"))(json).isEmpty)

  test("empty") {
    Seq(
      "{}",
      " \"yalla\"",
      "1",
      "true",
      "false",
      "[]",
      "[{}]"
    ).foreach(assertNoErrorsEmptyObj)
  }

  test("true") {
    val schema = parseSchema("true")
    val noErrs = Util.assertNoErrors(schema) _
    Seq(
      "{}",
      " \"yalla\"",
      "1",
      "true",
      "false",
      "[]",
      "[{}]"
    ).foreach(noErrs)
  }

  private def assertErrors(schema: String)(json: String, expected: Any): Unit =
    Util.assertErrors(parseSchema(schema))(json) { (_, e) => assertEquals(e, expected) }

  private def assertNoErrors(schema: String)(json: String): Unit =
    Util.assertErrors(parseSchema(schema))(json) { (_, e) => assert(e.isEmpty) }

  test("false") {
    val schema = parseSchema("false")
    Seq(
      "{}",
      "\"yalla\"",
      "1",
      "true",
      "false",
      "[]",
      "[{}]"
    ).foreach { s =>
      assertErrors("false")(s, Seq(NothingValidatesError(JPath.empty)))
    }
  }

  test("Long") {
    val noErrs = assertNoErrors(Examples.Long) _
    noErrs("0")
    noErrs("100")
    noErrs("-20")
    noErrs("null")
    assertErrors(Examples.Long)(
      "\"yalla\"",
      Seq(
        InvalidTypeError(JPath.empty, Seq("integer", "null"))
      )
    )
  }

  test("String") {
    val noErrs = assertNoErrors(Examples.String) _
    noErrs("\"\"")
    noErrs("\"yalla\"")
    noErrs("null")
    assertErrors(Examples.String)(
      "true",
      Seq(
        InvalidTypeError("/", Seq("string", "null"))
      )
    )
  }

  test("enum array") {
    val noErrs = assertNoErrors(Examples.EnumArray) _
    val errs   = assertErrors(Examples.EnumArray) _

    noErrs("null")
    noErrs("[]")
    noErrs("""["FOO"]""")
    noErrs("""["BAR"]""")
    noErrs("""["FOO", "BAR"]""")

    Seq("true", "1", "{}").foreach(s =>
      errs(
        s,
        Seq(
          InvalidTypeError("/", Seq("array", "null"))
        )
      )
    )

    errs(
      "[1]",
      List(
        InvalidTypeError("/", "null"),
        InvalidTypeError("/0", Seq("string", "null")),
        ValueNotInEnumError(
          "/0",
          List(Ast.Str(Ast.Position(126, 5), "FOO"), Ast.Str(Ast.Position(139, 5), "BAR"))
        )
      )
    )

    errs(
      """["yes"]""",
      List(
        InvalidTypeError("/", "null"),
        ValueNotInEnumError(
          "/0",
          List(Ast.Str(Ast.Position(126, 5), "FOO"), Ast.Str(Ast.Position(139, 5), "BAR"))
        )
      )
    )

    errs(
      """[null]""",
      List(
        InvalidTypeError("/", "null"),
        ValueNotInEnumError(
          "/0",
          List(Ast.Str(Ast.Position(126, 5), "FOO"), Ast.Str(Ast.Position(139, 5), "BAR"))
        )
      )
    )

  }

  test("Bean containing other bean") {
    val noErrs = assertNoErrors(Examples.BeanContainingOtherBean) _
    val errs   = assertErrors(Examples.BeanContainingOtherBean) _

    noErrs(
      """{
        |  "customer": {
        |  }
        |}""".stripMargin
    )
    noErrs(
      """{
        |  "customer": {
        |    "firstName": "yalla"
        |  }
        |}""".stripMargin
    )
    noErrs(
      """{
        |  "customer": {
        |    "firstName": null
        |  }
        |}""".stripMargin
    )
    errs(
      """{
        |  "customer": {
        |    "firstName": 123
        |  }
        |}""".stripMargin,
      List(
        InvalidTypeError("/customer/firstName", Seq("string", "null"))
      )
    )
  }

  test("Cycle") {
    val noErrs = assertNoErrors(Examples.Cycle) _
    val errs   = assertErrors(Examples.Cycle) _

    noErrs("""{}""")
    noErrs("""{ "next": {} }""")
    noErrs("""{ "next": { "next": {} } }""")
    noErrs("""{ "next": { "next": { "name": null } } }""")
    noErrs("""{ "next": { "next": { "name": "yalla" } } }""")
    noErrs("""{ "name": "yalla" }""")
    errs(
      """{"next":{"name":1}} """,
      List(
        InvalidTypeError("/next/name", Seq("string", "null"))
      )
    )
  }

  test("Inheritance") {
    val noErrs = assertNoErrors(Examples.Inheritance) _
    val errs   = assertErrors(Examples.Inheritance) _

    noErrs("{}")
    noErrs("""{ "height": 1 }""")
    noErrs("""{ "width": 2 }""")
    noErrs("""{ "height": 1, "width": 2, "name": "toto" }""")

    errs("""{ "height": "gni" }""", List(InvalidTypeError("/height", "integer")))
    errs(
      """{ "height": 1, "name": 123 }""",
      List(InvalidTypeError("/name", Seq("string", "null")))
    )
  }

  test("IfThenElse") {
    val schema =
      """{
        |  "$schema": "https://json-schema.org/draft/2019-09/schema",
        |  "type": "object",
        |  "if": {
        |    "properties": {
        |      "what": {
        |        "type": "string",
        |        "const": "funk"
        |      }
        |    },
        |    "required": [ "what" ]
        |  },
        |  "then": {
        |    "properties": {
        |      "groove": {
        |        "type": "string"
        |      }
        |    },
        |    "required": [ "groove" ]
        |  },
        |  "else": {
        |    "properties": {
        |      "soul": {
        |        "type": "string"
        |      }
        |    },
        |    "required": [ "soul" ]
        |  }
        |}""".stripMargin

    val noErrs = assertNoErrors(schema) _
    val errs   = assertErrors(schema) _
    noErrs("""{ "what": "funk", "groove": "yeah" }""")
    noErrs("""{ "what": "blues", "soul": "indeed" }""")
    noErrs("""{ "what": "musette", "soul": "nah" }""")
    errs("""{}""", Seq(RequiredPropertyError("/", Seq("soul"))))
    errs("""{ "what": "funk", "groove": true }""", Seq(InvalidTypeError("/groove", "string")))
    errs("""{ "what": "rnb", "soul": 123 }""", Seq(InvalidTypeError("/soul", "string")))
  }

  test("Polymorphism") {
    val noErrs = assertNoErrors(Examples.Polymorphism) _
    val errs   = assertErrors(Examples.Polymorphism) _

    noErrs("{}")
    noErrs(
      """{
        |  "what": "schema.animal.Lion",
        |  "mane": false
        |}""".stripMargin
    )
    noErrs(
      """{
        |  "what": "schema.animal.Elephant",
        |  "tusk": false
        |}""".stripMargin
    )
    errs(
      """{
        | "what": "schema.animal.Lion",
        | "mane": 123
        |}""".stripMargin,
      List(InvalidTypeError("/mane", "boolean"))
    )
    errs(
      """{
        | "what": "schema.animal.Lion",
        | "mane": true,
        | "sound": 123
        |}""".stripMargin,
      List(
        InvalidTypeError("/sound", Seq("string", "null"))
      )
    )
    errs(
      """{
        | "what": "schema.animal.Elephant",
        | "tusk": 123
        |}""".stripMargin,
      List(InvalidTypeError("/tusk", "boolean"))
    )
    errs(
      """{
        | "what": "schema.animal.Elephant",
        | "tusk": true,
        | "sound": 123
        |}""".stripMargin,
      List(InvalidTypeError("/sound", Seq("string", "null")))
    )
  }

  test("transitive refs") {
    val schema =
      """{
        |  "$ref": "#/definitions/A",
        |  "definitions": {
        |    "A": {
        |      "$ref": "#definitions/B"
        |    },
        |    "B": {
        |      "type": "object",
        |      "properties": {
        |        "bb": {
        |          "type": "string"
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin
    val noErrs = assertNoErrors(schema) _

    noErrs("{}")
    noErrs("""{ "bb": "yalla" }""")

    val errs = assertErrors(schema) _
    errs("""{ "bb": 123 }""", List(InvalidTypeError(JPath(List("bb")), "string")))
  }

  test("allOf and refs") {
    val schema =
      """{
        |  "allOf": [
        |    { "$ref": "#/definitions/A" },
        |    { "$ref": "#/definitions/B" }
        |  ],
        |  "definitions": {
        |    "A": {
        |      "type": "object",
        |      "properties": {
        |        "aa": {
        |          "type": "string"
        |        }
        |      }
        |    },
        |    "B": {
        |      "type": "object",
        |      "properties": {
        |        "bb": {
        |          "type": "string"
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin
    val noErrs = assertNoErrors(schema) _

    noErrs("{}")
    noErrs("""{ "aa": "yallaA" }""")
    noErrs("""{ "bb": "yallaB" }""")
    noErrs("""{ "aa": "yallaA", "bb": "yallaB" }""")

    val errs = assertErrors(schema) _
    errs("""{ "aa": "yallaA", "bb": 123 }""", List(InvalidTypeError(JPath(List("bb")), "string")))
    errs("""{ "aa": 123, "bb": "yallaB" }""", List(InvalidTypeError(JPath(List("aa")), "string")))
    errs(
      """{ "aa": 123, "bb": 456 }""",
      List(
        InvalidTypeError(JPath(List("aa")), "string"),
        InvalidTypeError(JPath(List("bb")), "string")
      )
    )
  }

  // Examples in https://json-schema.org/understanding-json-schema/

  test("dyn ref") {
    val schema =
      """{
        |  "$dynamicAnchor": "yalla",
        |  "properties": {
        |    "parent": { "$dynamicRef": "#yalla" }
        |  }
        |}""".stripMargin
    val errs   = assertErrors(schema) _
    errs("""{ "parent": 123 }""", List(InvalidTypeError("/parent", "object")))
  }

  test("additional props") {
    val schema =
      """{
        |  "additionalProperties": {
        |    "type": "string"
        |  }
        |}""".stripMargin
    val errs   = assertErrors(schema) _
    errs("""{ "x": 123, "y": "yeah" }""", List(InvalidTypeError("/x", "string")))
  }

  test("additional props false") {
    val schema =
      """{
        |  "properties": {
        |    "foo": { "type": "string" }
        |  },
        |  "additionalProperties": false
        |}""".stripMargin
    val errs   = assertErrors(schema) _
    errs("""{ "foo": "bar", "yalla": true }""", List(NothingValidatesError("/yalla")))
  }

  test("pattern props") {
    val schema = """{
                   |  "type": "object",
                   |  "patternProperties": {
                   |    "^S_": { "type": "string" },
                   |    "^I_": { "type": "integer" }
                   |  }
                   |}""".stripMargin
    val noErrs = assertNoErrors(schema) _
    noErrs("""{ "S_25": "This is a string" }""")
    noErrs("""{ "I_0": 42 }""")
    noErrs("""{ }""")
    noErrs("""{ "foo": 123 }""")
    val errs   = assertErrors(schema) _
    errs("""{ "S_nake": true }""", Seq(InvalidTypeError("/S_nake", "string")))
  }

  test("property names") {
    val schema = """{
                   |  "type": "object",
                   |  "propertyNames": {
                   |    "pattern": "^[A-Za-z_][A-Za-z0-9_]*$"
                   |  }
                   |}""".stripMargin
    assertNoErrors(schema)("""{
                             |  "_a_proper_token_001": "value"
                             |}""".stripMargin)
    assertErrors(schema)(
      """{
        |  "001 invalid": "value"
        |}""".stripMargin,
      Seq(
        PropertyNameError(
          "/",
          "001 invalid",
          Seq(PatternNotMatchingError("/", "^[A-Za-z_][A-Za-z0-9_]*$"))
        )
      )
    )
  }

  test("self-validate defs") {
    val schema = JsonSchemaSpec.schema
    val noErrs = Util.assertNoErrors(schema) _
    noErrs("{}")
    noErrs("""{ "foo": "bar" }""")
    val errs   = Util.assertErrors(schema) _
    errs("""{ "definitions": 123 }""") { (_, es) =>
      assert(es == List(InvalidTypeError("/", "boolean"), InvalidTypeError("/definitions", "object")))
    }
  }

  test("self-validate allOf") {
    val schema = JsonSchemaSpec.schema
    val errs   = Util.assertErrors(schema) _
    errs("""{ "allOf": 1 }""") { (_, es) =>
      assert(es == List(InvalidTypeError("/", "boolean"), InvalidTypeError("/allOf", "array")))
    }
  }

  private val schemaPattern: String =
    """{
      |  "type": "string",
      |  "pattern": "\\d+"
      |}""".stripMargin

  test("pattern ok") {
    assertErrors(schemaPattern)(
      "\"123\"",
      Seq.empty
    )
  }

  test("pattern ko") {
    assertErrors(schemaPattern)(
      "\"gnii\"",
      Seq(PatternNotMatchingError("/", "\\d+"))
    )
  }

}
