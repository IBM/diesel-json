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

import diesel.json.Ast._

class JsonTest extends AstTestFunSuite {

  private implicit val dsl: Json.type = Json

  testAst("null") {
    Null(Position(0, 4))
  }

  testAst("\"yalla\"") {
    Str(Position(0, 7), "yalla")
  }

  testAst("true") {
    Bool(Position(0, 4), v = true)
  }

  testAst("false") {
    Bool(Position(0, 5), v = false)
  }

  testAst("123") {
    Number(Position(0, 3), "123")
  }

  testAst("9999999999999999") {
    Number(Position(0, 16), "9999999999999999")
  }

  testAst("[1]") {
    Array(Position(0, 3), Seq(Number(Position(1, 1), "1")))
  }

  testAst("[1, true]") {
    Array(Position(0, 9), Seq(Number(Position(1, 1), "1"), Bool(Position(4, 4), v = true)))
  }

  testAst("[]") {
    Array(Position(0, 2), Seq.empty)
  }

  testAst("{}") {
    Object(Position(0, 2), Seq.empty)
  }

  testAst("""{ "foo": 123 }""") {
    Object(
      Position(0, 14),
      Seq(
        Attribute(Position(2, 10), AttrName(Position(2, 5), "foo"), Number(Position(9, 3), "123"))
      )
    )
  }

  testAst("""{ "foo": [1, 2, 3] }""") {
    Object(
      Position(0, 20),
      Seq(
        Attribute(
          Position(2, 16),
          AttrName(Position(2, 5), "foo"),
          Array(
            Position(9, 9),
            Seq(
              Number(Position(10, 1), "1"),
              Number(Position(13, 1), "2"),
              Number(Position(16, 1), "3")
            )
          )
        )
      )
    )
  }
}
