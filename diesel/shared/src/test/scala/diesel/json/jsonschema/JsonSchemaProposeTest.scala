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
import diesel.json.Ast.Constants._
import diesel.json.Ast.Builder._
import diesel.json.Ast.Builder.Implicits._
import JPath.Implicits._
import munit.FunSuite

class JsonSchemaProposeTest extends FunSuite {

  private def assertSchemaProposals(schema: String, expectedProposals: Seq[Ast.Value], maxDepth: Int = -1): Unit = {
    assertValidationProposals(
      schema,
      "",
      "/",
      expectedProposals,
      maxDepth
    )
  }

  private def assertValidationProposals(
    schema: String,
    json: String,
    path: JPath,
    expectedProposals: Seq[Ast.Value],
    maxDepth: Int = -1
  ): Unit = {
    val s         = JsonSchema.parse(schema).toOption.get
    val j         = Json.parse(json) match {
      case Json.JPRError(message)    =>
        fail(message)
      case Json.JPRSuccess(_, value) =>
        value
    }
    val vr        = s._2.validate(j)
    val proposals = JsonSchema.propose(vr, j, path, maxDepth)
    assert(
      proposals == expectedProposals
    )
  }

  test("schema propose bool") {
    assertSchemaProposals(
      """{ "type": "boolean" }""",
      Seq(bool(true), bool(false))
    )
  }

  test("schema propose object") {
    assertSchemaProposals(
      """{ "type": "object" }""",
      Seq(astObject)
    )
  }

  test("propose property") {
    val schema =
      """{
        |  "properties": {
        |    "foo": {
        |      "type": "string"
        |    }
        |  }
        |}""".stripMargin
    assertSchemaProposals(
      schema,
      Seq(
        obj(
          "foo" -> astNull
        )
      )
    )
    assertSchemaProposals(
      schema,
      Seq(
        obj(
          "foo" -> astStr
        )
      ),
      maxDepth = 1
    )
  }

  test("propose attr nested") {
    val schema =
      """{
        |  "properties": {
        |    "foo": {
        |      "$ref": "#/$defs/bar"
        |    }
        |  },
        |  "$defs": {
        |    "bar": {
        |      "properties": {
        |        "bar": {
        |          "type": "number"
        |        }
        |      }
        |    }
        |  }
        |}""".stripMargin
    assertSchemaProposals(
      schema,
      Seq(
        obj(
          "foo" -> astNull
        )
      )
    )
    assertSchemaProposals(
      schema,
      Seq(
        obj(
          "foo" -> obj(
            "bar" -> astNumber
          )
        )
      ),
      maxDepth = 2
    )
  }

  test("propose array") {
    val schema =
      """{
        |  "type": "array"
        |}""".stripMargin
    assertSchemaProposals(
      schema,
      Seq(astArray)
    )
  }

  test("at path nested") {
    val schema = """{
                   |  "properties": {
                   |    "foo": {
                   |      "$ref": "#/$defs/bar"
                   |    }
                   |  },
                   |  "$defs": {
                   |    "bar": {
                   |      "properties": {
                   |        "bar": {
                   |          "type": "number"
                   |        }
                   |      }
                   |    }
                   |  }
                   |}""".stripMargin
    val json   = """{ "foo" : {} }"""
    assertValidationProposals(
      schema,
      json,
      "/foo",
      Seq(obj("bar" -> astNull))
    )
    assertValidationProposals(
      schema,
      json,
      "/foo",
      Seq(obj("bar" -> astNumber)),
      maxDepth = 1
    )
  }

  test("propose at path array") {
    assertValidationProposals(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "number"
        |  }
        |}""".stripMargin,
      "[",
      "/0",
      Seq(astNumber)
    )
  }

  test("propose at path array 2") {
    assertValidationProposals(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "number"
        |  }
        |}""".stripMargin,
      "[1,",
      "/1",
      Seq(astNumber)
    )
  }

  test("examples Long") {
    assertSchemaProposals(
      Examples.Long,
      Seq(astNumber, astNull)
    )
  }

  test("examples String") {
    assertSchemaProposals(
      Examples.String,
      Seq(astStr, astNull)
    )
  }

  test("examples EnumArray 1") {
    assertSchemaProposals(
      Examples.EnumArray,
      Seq(astArray, astNull)
    )
  }

  test("examples EnumArray 2") {
    assertValidationProposals(
      Examples.EnumArray,
      "[]",
      "/0",
      Seq("FOO", "BAR")
    )
  }

  test("examples Cycle") {
    assertSchemaProposals(
      Examples.Cycle,
      Seq(
        obj(
          "name" -> astNull,
          "next" -> astNull
        )
      )
    )
    assertSchemaProposals(
      Examples.Cycle,
      Seq(
        obj(
          "name" -> astStr,
          "next" -> obj(
            "name" -> astNull,
            "next" -> astNull
          )
        )
      ),
      maxDepth = 1
    )
  }

  test("examples Poly 1") {
    assertValidationProposals(
      Examples.Polymorphism,
      """{}""",
      "/",
      List(
        astObject,
        obj("what"      -> astNull),
        obj("mane"      -> astNull),
        obj(
          "endangered"  -> astNull,
          "name"        -> astNull,
          "sound"       -> astNull,
          "type"        -> astNull
        ),
        obj(
          "trunkLength" -> astNull,
          "tusk"        -> astNull
        )
      )
    )
    assertValidationProposals(
      Examples.Polymorphism,
      """{}""",
      "/",
      List(
        astObject,
        obj("what"      -> str("schema.animal.Lion")),
        obj("mane"      -> astBool),
        obj(
          "endangered"  -> astBool,
          "name"        -> astStr,
          "sound"       -> astStr,
          "type"        -> astStr
        ),
        obj(
          "trunkLength" -> astNumber,
          "tusk"        -> astBool
        )
      ),
      maxDepth = 1
    )
  }

  test("examples Poly 2") {
    assertValidationProposals(
      Examples.Polymorphism,
      """{"what": ""}""",
      "/what",
      Seq("schema.animal.Lion", "schema.animal.Elephant")
    )
  }

  test("examples Poly 3") {
    assertValidationProposals(
      Examples.Polymorphism,
      """{"what": "schema.animal.Elephant"}""",
      "/",
      Seq(
        astObject,
        obj("what"      -> astNull),
        obj(
          "trunkLength" -> astNull,
          "tusk"        -> astNull
        ),
        obj(
          "endangered"  -> astNull,
          "name"        -> astNull,
          "sound"       -> astNull,
          "type"        -> astNull
        )
      )
    )
    assertValidationProposals(
      Examples.Polymorphism,
      """{"what": "schema.animal.Elephant"}""",
      "/",
      Seq(
        astObject,
        obj("what"      -> str("schema.animal.Lion")),
        obj(
          "trunkLength" -> astNumber,
          "tusk"        -> astBool
        ),
        obj(
          "endangered"  -> astBool,
          "name"        -> astStr,
          "sound"       -> astStr,
          "type"        -> astStr
        )
      ),
      maxDepth = 1
    )
  }

  test("examples Poly 4") {
    assertValidationProposals(
      Examples.Polymorphism,
      """{"what": "schema.animal.Lion"}""",
      "/",
      Seq(
        astObject,
        obj("what"     -> astNull),
        obj("mane"     -> astNull),
        obj(
          "endangered" -> astNull,
          "name"       -> astNull,
          "sound"      -> astNull,
          "type"       -> astNull
        )
      )
    )
    assertValidationProposals(
      Examples.Polymorphism,
      """{"what": "schema.animal.Lion"}""",
      "/",
      Seq(
        astObject,
        obj("what"     -> str("schema.animal.Lion")),
        obj("mane"     -> astBool),
        obj(
          "endangered" -> astBool,
          "name"       -> astStr,
          "sound"      -> astStr,
          "type"       -> astStr
        )
      ),
      maxDepth = 1
    )
  }

  test("propose nested") {
    assertValidationProposals(
      Examples.BeanContainingOtherBean,
      "null",
      "/",
      Seq(
        obj(
          "customer" -> astNull
        )
      )
    )
    assertValidationProposals(
      Examples.BeanContainingOtherBean,
      "null",
      "/",
      Seq(
        obj(
          "customer" -> obj(
            "age"       -> astNumber,
            "amount"    -> astNumber,
            "firstName" -> astStr,
            "lastName"  -> astStr
          )
        )
      ),
      maxDepth = 2
    )
  }

  test("propose enum array root") {
    assertValidationProposals(
      Examples.EnumArray,
      "null",
      "/",
      Seq(
        array(Seq.empty),
        astNull
      )
    )
  }

  test("do not propose empty string if there is an enum") {
    assertValidationProposals(
      Examples.EnumArrayWithoutNull,
      """[]""",
      "/0",
      Seq("FOO", "BAR")
    )
  }

  test("propose enum in array at index 1") {
    assertValidationProposals(
      Examples.EnumArrayWithoutNull,
      """["FOO", null]""",
      "/1",
      Seq("FOO", "BAR")
    )
  }

  test("propose enum in array at invalid index") {
    assertValidationProposals(
      Examples.EnumArrayWithoutNull,
      """["FOO"]""",
      "/1",
      Seq()
    )
  }

  test("propose with default") {
    assertValidationProposals(
      """{
        | "type": "object",
        | "properties": {
        |   "foo": {
        |     "type": "string"
        |   }
        | },
        | "default": {
        |   "foo": "bar"
        | }
        |}""".stripMargin,
      """{}""",
      "/",
      Seq(
        obj(
          "foo" -> str("bar")
        )
      )
    )
  }

  test("propose with one example") {
    assertValidationProposals(
      """{
        | "type": "object",
        | "properties": {
        |   "foo": {
        |     "type": "string"
        |   }
        | },
        | "examples": [
        |   {
        |     "foo": "bar"
        |   }
        | ]
        |}""".stripMargin,
      """{}""",
      "/",
      Seq(
        obj(
          "foo" -> str("bar")
        )
      )
    )
  }

  test("propose with two examples") {
    assertValidationProposals(
      """{
        | "type": "object",
        | "properties": {
        |   "foo": {
        |     "type": "string"
        |   }
        | },
        | "examples": [
        |   {
        |     "foo": "bar"
        |   },
        |   {
        |     "foo": "baz"
        |   }
        | ]
        |}""".stripMargin,
      """{}""",
      "/",
      Seq(
        obj(
          "foo" -> str("bar")
        ),
        obj(
          "foo" -> str("baz")
        )
      )
    )
  }

  test("propose with examples and default") {
    assertValidationProposals(
      """{
        | "type": "object",
        | "properties": {
        |   "foo": {
        |     "type": "string"
        |   }
        | },
        | "examples": [
        |   {
        |     "foo": "bar"
        |   },
        |   {
        |     "foo": "baz"
        |   }
        | ],
        | "default": {
        |   "foo": "yalla"
        | }
        |}""".stripMargin,
      """{}""",
      "/",
      Seq(
        obj(
          "foo" -> str("yalla")
        ),
        obj(
          "foo" -> str("bar")
        ),
        obj(
          "foo" -> str("baz")
        )
      )
    )
  }

}
