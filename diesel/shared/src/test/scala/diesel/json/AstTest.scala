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

import diesel.json.Ast.Value
import diesel.json.Ast.Constants.{astNull, astNumber}
import diesel.json.jsonschema.JPath.Implicits._
import diesel.json.Ast.Builder._
import diesel.json.Ast.Builder.Implicits._
import munit.FunSuite

class AstTest extends FunSuite {

  test("root") {
    assert(
      Value.map(astNull, "/") { _ => astNumber } == astNumber
    )
  }

  test("nested") {
    assert(
      Value.map(
        obj(
          "foo" -> astNull
        ),
        "/foo"
      )(_ => astNumber) == obj("foo" -> astNumber)
    )
  }

  test("nested 2") {
    assert(
      Value.map(
        obj(
          "foo" -> obj(
            "bar" -> astNull
          )
        ),
        "/foo/bar"
      )(_ => astNumber) == obj("foo" -> obj("bar" -> astNumber))
    )
  }

  test("array") {
    assert(
      Value.map(
        array(astNull),
        "/0"
      )(_ => astNumber) == array(astNumber)
    )
  }

}
