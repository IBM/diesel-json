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

import diesel.Marker
import diesel.Marker.{Descriptor, Kind, Severity}
import diesel.json.i18n.I18n
import diesel.json.jsonschema.{Examples, JsonSchema}
import diesel.i18n.Messages
import munit.FunSuite

class JsonValidatorTest extends FunSuite {

  implicit val en: Messages.KeyResolver = I18n.currentKeyResolver

  private def validate(schema: Option[JsonSchema], text: String): Seq[Marker] = {
    Json.parse(text) match {
      case Json.JPRError(message)   =>
        fail(message)
      case Json.JPRSuccess(tree, _) =>
        val schemaMarkers = schema
          .map(s => JsonSchema.postProcessMarkers(s)(tree))
          .getOrElse(Seq.empty)
        tree.markers ++ schemaMarkers
    }
  }

  private def assertMarkers(schema: Option[String], text: String, expectedMarkers: Seq[Marker]): Unit = {
    val s      = schema.map { schemaStr =>
      JsonSchema.parse(schemaStr) match {
        case Left(err)    =>
          fail(err)
        case Right(value) =>
          value._2
      }
    }
    val actual = validate(s, text)
    assert(actual == expectedMarkers)
  }

  private def parseSchema(text: String): JsonSchema =
    JsonSchema.parse(text).toOption.get._2

  test("no errors") {
    assertMarkers(None, "{}", Seq.empty)
  }

  import diesel.MarkerMessage.Implicits.strToMsg

  test("duplicated attributes") {
    assertMarkers(
      None,
      """{
        | "foo": 1,
        | "foo": 2
        |}""".stripMargin,
      List(
        Marker(Descriptor(Kind.Semantic, Severity.Error), 3, 5, "Duplicated attribute foo"),
        Marker(Descriptor(Kind.Semantic, Severity.Error), 14, 5, "Duplicated attribute foo")
      )
    )
  }

  test("duplicated attributes (nested)") {
    assertMarkers(
      None,
      """{
        | "foo": {
        |   "x": "a",
        |   "y": "b",
        |   "x": "c"
        | },
        | "bar": 2
        |}""".stripMargin,
      List(
        Marker(Descriptor(Kind.Semantic, Severity.Error), 15, 3, "Duplicated attribute x"),
        Marker(Descriptor(Kind.Semantic, Severity.Error), 41, 3, "Duplicated attribute x")
      )
    )
  }

  test("duplicated attributes (array)") {
    assertMarkers(
      None,
      """[
        |{ "foo": 1, "foo": 2 },
        |{ "x": 3, "y": 4, "x": 5 }
        |]""".stripMargin,
      List(
        Marker(Descriptor(Kind.Semantic, Severity.Error), 4, 5, "Duplicated attribute foo"),
        Marker(Descriptor(Kind.Semantic, Severity.Error), 14, 5, "Duplicated attribute foo"),
        Marker(Descriptor(Kind.Semantic, Severity.Error), 28, 3, "Duplicated attribute x"),
        Marker(Descriptor(Kind.Semantic, Severity.Error), 44, 3, "Duplicated attribute x")
      )
    )
  }

  private val customerSchema =
    """{
      |  "properties": {
      |    "name": {
      |      "type": "string"
      |    },
      |    "rating": {
      |      "type": "integer"
      |    },
      |    "address": {
      |      "properties": {
      |        "street": {
      |          "type": "string"
      |        }
      |      },
      |      "required": [ "street" ]
      |    }
      |  },
      |  "required": [
      |    "name", "address"
      |  ]
      |}""".stripMargin

  private def invalidTypeMsg(expected: String) = I18n.invalidType(Seq(expected))
  private val expectedObject                   = invalidTypeMsg("object")

  test("Unexpected value (root)") {
    assertMarkers(
      Some(customerSchema),
      """1""".stripMargin,
      Seq(
        err(0, 1, expectedObject)
      )
    )
  }

  test("Unexpected value (nested)") {
    assertMarkers(
      Some(customerSchema),
      """{
        | "name": "yalla",
        | "address": 123
        |}""".stripMargin,
      Seq(
        err(32, 3, expectedObject)
      )
    )
  }

  private val jsArrayNumbers =
    """{
      |  "type": "array",
      |  "items": {
      |    "type": "number"
      |  }
      |}""".stripMargin

  test("Unexpected value (array)") {
    assertMarkers(
      Some(jsArrayNumbers),
      """123""",
      Seq(
        err(0, 3, invalidTypeMsg("array"))
      )
    )
  }

  private val expectedNumber = invalidTypeMsg("number")

  test("Unexpected value (array elem)") {
    assertMarkers(
      Some(jsArrayNumbers),
      """[1, "foo", true]""",
      Seq(
        err(4, 5, expectedNumber),
        err(11, 4, expectedNumber)
      )
    )
  }

  private def missingProperty(name: String) = I18n.missingRequiredProperties(1)(Seq(name))

  test("Missing attribute") {
    assertMarkers(
      Some(customerSchema),
      """{
        |  "name": "yalla"
        |}""".stripMargin,
      Seq(
        err(0, 21, missingProperty("address"))
      )
    )
  }

  test("Missing attribute (nested)") {
    assertMarkers(
      Some(customerSchema),
      """{
        |  "name": "yalla",
        |  "address": {}
        |}""".stripMargin,
      Seq(
        err(34, 2, missingProperty("street"))
      )
    )
  }

  private val domainValues = Seq("bad", "medium", "good")

  private val schemaWithDomain =
    s"""{
       |  "type": "string",
       |  "enum": [ ${domainValues.map(s => "\"" + s + "\"").mkString(", ")} ]
       |}""".stripMargin

  test("Invalid domain value") {
    assertMarkers(
      Some(schemaWithDomain),
      """"foo"""",
      Seq(
        err(0, 5, """Invalid value: should be one of "bad" | "medium" | "good"""")
      )
    )
  }

  private val shapeSchema =
    """{
      |  "allOf": [
      |    { "$ref": "#/definitions/Shape" },
      |    {
      |      "oneOf": [
      |        { "$ref": "#/definitions/Circle" },
      |        { "$ref": "#/definitions/Rectangle" }
      |      ]
      |    }
      |  ],
      |  "definitions": {
      |    "Shape": {
      |      "properties": {
      |        "area": {
      |          "type": "number"
      |        }
      |      },
      |      "required": [ "area" ]
      |    },
      |    "Rectangle": {
      |      "properties": {
      |        "height": {
      |          "type": "number"
      |        },
      |        "width": {
      |          "type": "number"
      |        }
      |      },
      |      "required": [ "height", "width" ]
      |    },
      |    "Circle": {
      |      "properties": {
      |        "radius": {
      |          "type": "number"
      |        }
      |      },
      |      "required": [ "radius" ]
      |    }
      |  }
      |}""".stripMargin

  private def missingProperties(names: Seq[String]) = I18n.missingRequiredProperties(names.length)(names)
  test("shape") {
    assertMarkers(
      Some(shapeSchema),
      """{ "area": 12 }""",
      Seq(err(0, 14, missingProperties(Seq("radius", "height", "width"))))
    )
  }

  private val ifThenElseSchema =
    """{
      |  "if": {
      |    "properties": {
      |      "kind": {
      |        "type": "string",
      |        "const": "foo"
      |      }
      |    }
      |  },
      |  "then": {
      |    "properties": {
      |      "x": {
      |        "type": "string"
      |      }
      |    },
      |    "required": [ "x" ]
      |  },
      |  "else": {
      |    "properties": {
      |      "y": {
      |        "type": "number"
      |      }
      |    },
      |    "required": [ "y" ]
      |  }
      |}""".stripMargin

  test("if then else") {
    assertMarkers(
      Some(ifThenElseSchema),
      """{
        | "kind": "foo"
        |}""".stripMargin,
      Seq(err(0, 18, missingProperty("x")))
    )
    assertMarkers(
      Some(ifThenElseSchema),
      """{
        | "kind": "bar"
        |}""".stripMargin,
      Seq(err(0, 18, missingProperty("y")))
    )
  }

  private def err(offset: Int, lengh: Int, message: String): Marker = Marker(
    Descriptor(Kind.Semantic, Severity.Error),
    offset,
    lengh,
    message
  )

  test("shape schema") {
    assertMarkers(
      Some(Examples.ShapeSchema),
      """{}""",
      Seq(
        err(0, 2, missingProperties(Seq("height", "width", "radius")))
      )
    )
  }

  test("shape schema 2") {
    assertMarkers(
      Some(Examples.ShapeSchema),
      """{
        |  "area": 123,
        |  "radius": 456
        |}""".stripMargin,
      Seq.empty
    )
  }

  test("shape schema 3") {
    assertMarkers(
      Some(Examples.ShapeSchema),
      """{
        |  "area": 123,
        |  "height": 456,
        |  "width": 789
        |}""".stripMargin,
      Seq.empty
    )
  }

  test("shape schema 4") {
    assertMarkers(
      Some(Examples.ShapeSchema),
      """{
        |  "area": 123,
        |  "width": 789
        |}""".stripMargin,
      Seq(
        err(0, 33, missingProperties(Seq("height", "radius")))
      )
    )
  }

  test("no dups") {
    assertMarkers(
      Some(
        """{
          |  "type" : "string"
          |}""".stripMargin
      ),
      """true""",
      Seq(
        err(0, 4, invalidTypeMsg("string"))
      )
    )
  }

  test("no dups 2") {

    val schema = """{
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
                   |}""".stripMargin
    val value  = """{
                  |    "foo": {
                  |      "bar": 123,
                  |      "baz": "yalla"
                  |    },
                  |    "blah": true
                  |}""".stripMargin
    assertMarkers(
      Some(schema),
      value,
      Seq(
        err(73, 4, invalidTypeMsg("string"))
      )
    )
  }

}
