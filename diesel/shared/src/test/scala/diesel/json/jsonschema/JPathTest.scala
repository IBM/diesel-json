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
import diesel.json.jsonschema.Util.parseJson
import munit.FunSuite

class JPathTest extends FunSuite {

  test("root") {
    val x = parseJson("{}")
    val v = "/".resolve(x)
    assert(v.get == x)
  }

  test("attr") {
    val x = parseJson("""{"foo": 1}""")
    val v = JPath.parsePath("/foo").resolve(x)
    assert(v.get.asInstanceOf[Ast.Number].v == "1")
  }

  test("array") {
    val x = parseJson("""[1, 2, 3]""")
    val v = "/1".resolve(x)
    assert(v.get.asInstanceOf[Ast.Number].v == "2")
  }

  test("array attr") {
    val x      = parseJson(
      """[
        |  {
        |    "foo": 1
        |  },
        |  2,
        |  true
        |]""".stripMargin
    )
    val fooVal = x.asAstArray
      .get
      .elemAt(0)
      .get
      .asAstObject
      .get
      .attr("foo")
      .get
      .asAstNumber
      .get

    assert(
      "0/foo".resolve(x).get == fooVal
    )
  }

  test("not found") {
    val x = parseJson("""{"foo": 1}""")

    def assertNotFound(p: JPath) =
      assert(p.resolve(x).isEmpty)

    assertNotFound("/yalla")
    assertNotFound("/a/b/c")
    assertNotFound("/123")
  }

  test("array of array") {
    val x = parseJson(
      """[
        |  "foo",
        |  [
        |    true,
        |    [
        |      1,
        |      2,
        |      3
        |    ],
        |    false
        |  ],
        |  "bar"
        |]""".stripMargin
    )

    assert(
      "/1/1/1".resolve(x).flatMap(_.asAstNumber).exists(_.v == "2")
    )
  }

}
