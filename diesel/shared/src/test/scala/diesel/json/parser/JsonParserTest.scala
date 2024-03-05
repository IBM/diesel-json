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

package diesel.json.parser

import munit.FunSuite
import JsonParser._

class JsonParserTest extends FunSuite {

  private def assertParse(text: String, expected: Either[String, Value]) = {
    val p = JsonParser.parse(text)
    assertEquals(p, expected)
  }

  test("num") {
    assertParse("1", Right(VNumber("1")))
  }

  test("bool") {
    assertParse("true", Right(VBool(true)))
  }

  test("array empty") {
    assertParse("[]", Right(VArray(Seq.empty)))
  }

  test("array one value") {
    assertParse("[1]", Right(VArray(Seq(VNumber("1")))))
  }

  test("array two values") {
    assertParse("[1,true]", Right(VArray(Seq(VNumber("1"), VBool(true)))))
  }

  test("array multiple commas") {
    assertParse("[1,,2]", Left("Unexpected comma at offset 3"))
  }

  test("array trailing comma") {
    assertParse("[1,]", Left("Trailing comma at offset 2"))
  }

  test("array not closed") {
    assertParse("[1,", Left("Unexpected eos at offset 3"))
  }

  test("array nested") {
    assertParse("[[[]], []]", Right(VArray(Seq(VArray(Seq(VArray(Seq.empty))), VArray(Seq.empty)))))
  }

  test("object empty") {
    assertParse("{}", Right(VObject(Seq.empty)))
  }

  test("object one attr") {
    assertParse("""{ "x" : 1 }""", Right(VObject(Seq(("x" -> VNumber("1"))))))
  }

  test("object two attrs") {
    assertParse("""{ "x" : 1, "y": 2 }""", Right(VObject(Seq("x" -> VNumber("1"), "y" -> VNumber("2")))))
  }

  test("object trailing comma") {
    assertParse("""{ "x" : 1, }""", Left("Trailing comma at offset 9"))
  }

  test("object non closed attr 1") {
    assertParse("""{ " }""", Left("Invalid token at offset 2"))
  }

  test("object non closed attr 2") {
    assertParse("""{ "x }""", Left("Invalid token at offset 2"))
  }

  test("object non closed attr 3") {
    assertParse("""{ "x" }""", Left("Unexpected token at 6, expected SemiColon, found CloseObject"))
  }

  test("object non closed attr 4") {
    assertParse("""{ "x" : }""", Left("Unexpected token '}' at 8"))
  }

  test("object nested") {
    assertParse(
      """{
        "x": {
          "y": 123
        }
      }""",
      Right(
        VObject(
          Seq(
            "x" -> VObject(
              Seq(
                "y" -> VNumber("123")
              )
            )
          )
        )
      )
    )
  }

  test("mix") {
    assertParse(
      """{
        "x": [
          1, 
          2,
          { 
            "y": true,
            "z": "yalla"
          }
        ]
      }""",
      Right(
        VObject(
          Seq(
            "x" -> VArray(
              Seq(
                VNumber("1"),
                VNumber("2"),
                VObject(
                  Seq(
                    "y" -> VBool(true),
                    "z" -> VString("\"yalla\"")
                  )
                )
              )
            )
          )
        )
      )
    )
  }

}
