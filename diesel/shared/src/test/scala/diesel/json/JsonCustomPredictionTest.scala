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

package diesel.json

import diesel.AstHelpers.predict
import diesel.json.jsonschema.{Examples, JsonSchema}
import munit.FunSuite

class JsonCustomPredictionTest extends FunSuite {

  private def dblQuoted(s: String) = "\"" + s + "\""

  private val emptyString = dblQuoted("")
  private val address     = dblQuoted("address")
  private val street      = dblQuoted("street")
  private val rating      = dblQuoted("rating")
  private val name        = dblQuoted("name")

  private val customerSchema =
    """{
      |  "properties": {
      |    "name": {
      |      "type": "string"
      |    },
      |    "rating": {
      |      "type": "number"
      |    },
      |    "address": {
      |      "properties": {
      |        "street": {
      |          "type": "string"
      |        }
      |      }
      |    }
      |  },
      |  "required": [ "name", "address" ]
      |}""".stripMargin

  def assertPredictions(
    schema: String,
    text: String,
    offset: Int,
    expectedPredictions: Seq[String],
    expectedReplacements: Option[Seq[Option[(Int, Int)]]] = None
  ): Unit = {
    val s         = JsonSchema.parse(schema).toOption.get._2
    val config    = JsonCompletion.completionConfiguration(s)
    val proposals = predict(Json, text, offset, Some(config))
    assertEquals(proposals.map(_.text), expectedPredictions)
    assertEquals(
      proposals.map(_.replace),
      expectedReplacements.getOrElse(expectedPredictions.map(_ => None))
    )
  }

  private val expectedObjectFirstLevel = Seq("}", address, name, rating, emptyString)

  test("object first level, incomplete") {
    assertPredictions(
      customerSchema,
      "{",
      1,
      expectedObjectFirstLevel
    )
  }

  test("object first level, closed") {
    assertPredictions(
      customerSchema,
      "{}",
      1,
      expectedObjectFirstLevel
    )
  }

  test("object first level, inside quotes") {
    assertPredictions(
      customerSchema,
      "{ \"\"",
      3,
      expectedObjectFirstLevel,
      Some(expectedObjectFirstLevel.map(_ => Some((2, 1))))
    )
  }

  test("object first level, incomplete quote".ignore) {
    assertPredictions(
      customerSchema,
      "{\"",
      2,
      Seq(address, name, rating, emptyString)
    )
  }

  test("object first level, no whitespace after curly") {
    assertPredictions(
      customerSchema,
      "{",
      offset = 1,
      Seq("}", address, name, rating, emptyString)
    )
  }

  private val expectedAfterComma = Seq("}", address, rating, emptyString)

  test("fields are predicted after comma") {
    assertPredictions(
      customerSchema,
      """{ "name": "foo",""",
      16,
      expectedAfterComma
    )
  }

  test("fields are predicted after comma (no ws)") {
    val expected = Seq(emptyString, address, rating)
    assertPredictions(
      customerSchema,
      """{"name":"foo",""",
      14,
      expectedAfterComma
    )
  }

  test("fields are predicted after comma (complete 1)") {
    val expected = Seq(emptyString, address, rating)
    assertPredictions(
      customerSchema,
      """{ "name": "foo",}""",
      16,
      expectedAfterComma
    )
  }

  test("fields are predicted after comma (complete 2)") {
    val expected = Seq(emptyString, address, rating)
    assertPredictions(
      customerSchema,
      """{ "name": "foo", }""",
      17,
      expectedAfterComma
    )
  }

  test("fields are predicted after comma (complete 3)") {
    assertPredictions(
      customerSchema,
      """{ "name": "foo",  }""",
      17,
      expectedAfterComma
    )
  }

  private val expectedAfterField = Seq(",", "}")

  test("comma is predicted after field") {
    val expected = Seq(emptyString, address, rating)
    assertPredictions(
      customerSchema,
      """{ "name": "foo" """,
      16,
      expectedAfterField
    )
  }

  test("comma is predicted after field (complete)") {
    val expected = Seq(emptyString, address, rating)
    assertPredictions(
      customerSchema,
      """{ "name": "foo"  }""",
      16,
      expectedAfterField
    )
  }

  test("fields already present are not predicted") {
    assertPredictions(
      customerSchema,
      """{ "name": "foo",  }""",
      16,
      Seq("}", address, rating, emptyString)
    )
  }

  test("object nested") {
    assertPredictions(
      customerSchema,
      """{"address":{}}""",
      12,
      Seq("}", street, emptyString)
    )
  }

  test("array of numbers") {
    assertPredictions(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "number"
        |  }
        |}""".stripMargin,
      "[",
      1,
      Seq("0", "]")
    )
  }

  test("array of numbers 2") {
    assertPredictions(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "number"
        |  }
        |}""".stripMargin,
      "[1]",
      1,
      Seq("0", "]")
    )
  }

  test("array of strings") {
    assertPredictions(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "string"
        |  }
        |}""".stripMargin,
      "[",
      1,
      Seq(emptyString, "]")
    )
  }

  test("array of bools") {
    assertPredictions(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "boolean"
        |  }
        |}""".stripMargin,
      "[",
      1,
      Seq("true", "false", "]")
    )
  }

  test("array of objects") {
    assertPredictions(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "object"
        |  }
        |}""".stripMargin,
      "[",
      1,
      Seq("{", "]")
    )
  }

  test("array of array of number") {
    assertPredictions(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "array",
        |    "items": {
        |      "type": "number"
        |    }
        |  }
        |}""".stripMargin,
      "[",
      1,
      Seq("[", "]")
    )
  }

  private val schemaArrayAttribute = """{
                                       |  "properties": {
                                       |    "foo": {
                                       |      "type": "array",
                                       |      "items": {
                                       |        "type": "number"
                                       |      }
                                       |    }
                                       |  }
                                       |}""".stripMargin

  test("array attribute") {
    assertPredictions(
      schemaArrayAttribute,
      "{\"foo\":",
      7,
      Seq("[")
    )
  }

  test("array attribute predict after space") {
    assertPredictions(
      schemaArrayAttribute,
      "{\"foo\": }",
      8,
      Seq("[")
    )
  }

  test("array elem attribute") {
    assertPredictions(
      schemaArrayAttribute,
      "{\"foo\":[",
      8,
      Seq("0", "]")
    )
  }

  test("object inside array") {
    assertPredictions(
      """{
        |  "type": "array",
        |  "items": {
        |    "properties": {
        |      "foo": {
        |        "type": "number"
        |      }
        |    }
        |  }
        |}""".stripMargin,
      "[{}]",
      2,
      Seq("}", "\"foo\"", emptyString)
    )
  }

  test("no required fields should allow empty object") {
    assertPredictions(
      """{
        |  "properties": {
        |    "foo": {
        |      "type": "number"
        |    },
        |    "bar": {
        |      "type": "number"
        |    }
        |  }
        |}""".stripMargin,
      "{}",
      1,
      Seq("}", "\"bar\"", "\"foo\"", emptyString)
    )
  }

  private val rootExpected = Seq("0", "true", "false", "[", "{", emptyString)

  test("root number") {
    assertPredictions(
      """{
        |  "type": "number"
        |}""".stripMargin,
      "",
      0,
      Seq("0")
    )
  }

  test("root number 2") {
    assertPredictions(
      """{
        |  "type": "number"
        |}""".stripMargin,
      "123",
      0,
      Seq("0")
    )
  }

  test("root string") {
    assertPredictions(
      """{
        |  "type": "string"
        |}""".stripMargin,
      "",
      0,
      Seq(emptyString)
    )
  }

  test("root array") {
    assertPredictions(
      """{
        |  "type": "array"
        |}""".stripMargin,
      "",
      0,
      Seq("[")
    )
  }

  test("root object") {
    assertPredictions(
      """{
        |  "type": "object"
        |}""".stripMargin,
      "",
      0,
      Seq("{")
    )
  }

  test("root object 2") {
    assertPredictions(
      """{
        |  "type": "object"
        |}""".stripMargin,
      "{}",
      0,
      Seq("{")
    )
  }

  private val domainValues = Seq("bad", "medium", "good")
//  private val domainRating = JsDomain("Rating", () => domainValues)

  test("root domain") {
    assertPredictions(
      """{
        |  "type": "string",
        |  "enum": [
        |    "bad", "medium", "good"
        |  ]
        |}""".stripMargin,
      "",
      offset = 0,
      List("\"bad\"", "\"medium\"", "\"good\"")
    )
  }

  test("array of domains") {
    assertPredictions(
      """{
        |  "type": "array",
        |  "items": {
        |    "type": "string",
        |    "enum": [ "bad", "medium", "good" ]
        |  }
        |}""".stripMargin,
      "[]",
      offset = 1,
      List("\"bad\"", "\"medium\"", "\"good\"", "]")
    )
  }

  test("domain attribute") {
    assertPredictions(
      """{
        |  "properties": {
        |    "rating": {
        |      "type": "string",
        |      "enum": [ "bad", "medium", "good" ]
        |    }
        |  }
        |}""".stripMargin,
      "{\"rating\":",
      offset = 10,
      List("\"bad\"", "\"medium\"", "\"good\"")
    )
  }

  test("predict object attr 1") {
    assertPredictions(
      customerSchema,
      """{"address":""",
      offset = 11,
      Seq("{")
    )
  }

  test("predict object attr 2") {
    assertPredictions(
      customerSchema,
      """{"name":""",
      offset = 8,
      Seq(emptyString)
    )
  }

  test("predict object attr 3".ignore) {
    assertPredictions(
      customerSchema,
      """{"name": """,
      offset = 8,
      Seq(emptyString)
    )
  }

  test("predict after attr") {
    assertPredictions(
      Examples.BeanContainingOtherBean,
      """{"customer" """,
      offset = 12,
      Seq(":")
    )
  }

  test("customer age 1") {
    assertPredictions(
      Examples.BeanContainingOtherBean,
      """{ "customer" : { "age" : """,
      offset = 25,
      Seq("0")
    )
  }

  test("customer age 2") {
    assertPredictions(
      Examples.BeanContainingOtherBean,
      """{ "customer" : { "age" :  """,
      offset = 25,
      Seq("0")
    )
  }

  test("customer age 3") {
    assertPredictions(
      Examples.BeanContainingOtherBean,
      """{ "customer" : { "age" :  }""",
      offset = 25,
      Seq("0")
    )
  }

  test("customer age 4") {
    assertPredictions(
      Examples.BeanContainingOtherBean,
      """{ "customer" : { "age" :      }""",
      offset = 25,
      Seq("0")
    )
  }

  test("super class shape") {
    assertPredictions(
      Examples.ShapeSchema,
      "{",
      offset = 1,
      Seq("}", "\"area\"", "\"height\"", "\"width\"", "\"radius\"", emptyString)
    )
  }

  test("predict object attr names") {
    assertPredictions(
      customerSchema,
      """{""",
      offset = 1,
      Seq("}", "\"address\"", "\"name\"", "\"rating\"", "\"\"")
    )
  }

  test("predict object with existing attr names, after") {
    assertPredictions(
      customerSchema,
      """{ "name": "Toto", """,
      offset = 17,
      Seq("}", "\"address\"", "\"rating\"", "\"\"")
    )
  }

  test("predict object with existing attr names, before") {
    assertPredictions(
      customerSchema,
      """{ "name": "Toto", """,
      offset = 1,
      Seq("}", "\"address\"", "\"rating\"", "\"\"")
    )
  }

  test("object props") {
    assertPredictions(
      """{
        |      "type": "object",
        |      "properties": {
        |        "foo": {
        |          "type": "string",
        |        },
        |      },
        |}""".stripMargin,
      "{}",
      offset = 1,
      Seq("}", "\"\"", "\"foo\"", "\"\"")
    )
  }
}
