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

import munit.FunSuite
import diesel.json.JsonParser.JPRError
import diesel.json.JsonParser.JPRSuccess
import diesel.json.Ast.Str
import diesel.json.Ast.Position
import diesel.json.Ast.Bool

class JsonParserTest extends FunSuite {

  private def doTest(text: String, expected: Ast.Value): Unit = {
    JsonParser.parse(text) match {
      case JPRError(message) =>
        fail(message)
      case JPRSuccess(value) =>
        println(value)
        assertEquals(value, expected)
        Json.parseWithDsl(text) match {
          case Left(message)   =>
            fail(message)
          case Right(valueDsl) =>
            assertEquals(value, valueDsl._2)
        }
    }
  }

  test("string") {
    val text = "\"yalla\""
    doTest(text, Str(Position(0, text.length()), "yalla"))
  }

  test("number") {
    val text = "123"
    doTest(text, Ast.Number(Position(0, text.length()), "123"))
  }

  test("boolean true") {
    val text = "true"
    doTest(text, Bool(Position(0, text.length()), true))
  }

  test("boolean false") {
    val text = "false"
    doTest(text, Bool(Position(0, text.length()), false))
  }

  test("null") {
    val text = "null"
    doTest(text, Ast.Null(Position(0, text.length())))
  }

  test("array") {
    val text = "[1,2]"
    doTest(
      text,
      Ast.Array(
        position = Position(
          offset = 0,
          length = 5
        ),
        elems = List(
          Ast.Number(
            position = Position(
              offset = 1,
              length = 1
            ),
            v = "1"
          ),
          Ast.Number(
            position = Position(
              offset = 3,
              length = 1
            ),
            v = "2"
          )
        )
      )
    )
  }

  test("object") {
    val text = """{"foo":123}"""
    doTest(
      text,
      Ast.Object(
        position = Position(
          offset = 0,
          length = 11
        ),
        attributes = List(
          Ast.Attribute(
            position = Position(
              offset = 1,
              length = 9
            ),
            name = Ast.AttrName(
              position = Position(
                offset = 1,
                length = 5
              ),
              s = "foo"
            ),
            value = Ast.Number(
              position = Position(
                offset = 7,
                length = 3
              ),
              v = "123"
            )
          )
        )
      )
    )
  }

  test("object with str prop") {
    val text = """{"foo":"bar"}"""
    doTest(
      text,
      Ast.Object(
        position = Position(
          offset = 0,
          length = 13
        ),
        attributes = List(
          Ast.Attribute(
            position = Position(
              offset = 1,
              length = 11
            ),
            name = Ast.AttrName(
              position = Position(
                offset = 1,
                length = 5
              ),
              s = "foo"
            ),
            value = Ast.Str(
              position = Position(
                offset = 7,
                length = 5
              ),
              v = "bar"
            )
          )
        )
      )
    )
  }

  test("string array") {
    doTest(
      """["foo"]""",
      Ast.Array(
        position = Position(
          offset = 0,
          length = 7
        ),
        elems = List(
          Str(
            position = Position(
              offset = 1,
              length = 5
            ),
            v = "foo"
          )
        )
      )
    )
  }

}
