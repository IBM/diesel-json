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
import diesel.CompletionConfiguration
import munit.FunSuite

class JsonPredictionTest extends FunSuite {

  def assertPredictions(
    text: String,
    offset: Int,
    expectedPredictions: Seq[String]
  ): Unit = {
    val config    = new CompletionConfiguration()
    config.setLookback(JsonCompletion.completionLookback)
    val proposals = predict(Json, text, offset, Some(config))
    assert(
      proposals.map(_.text) == expectedPredictions
    )
    assert(
      proposals.map(_.replace) == expectedPredictions.map(_ => None)
    )
  }

  private val allValues = List("null", "0", "true", "false", "[", "{", "\"\"")

  test("empty") {
    assertPredictions(
      "",
      0,
      allValues
    )
  }

  test("object open") {
    assertPredictions(
      "{",
      1,
      Seq("}", "\"\"")
    )
  }

  test("object closed") {
    assertPredictions(
      "{}",
      1,
      Seq("}", "\"\"")
    )
  }

  test("object closed with spaces") {
    assertPredictions(
      "{    }",
      3,
      Seq("}", "\"\"")
    )
  }

  test("object attr value") {
    assertPredictions(
      """{ "x": """,
      6,
      allValues
    )
  }

  test("object attr value predicts comma") {
    assertPredictions(
      """{ "x": 0 """,
      9,
      Seq(",", "}")
    )
  }

  test("object attr value predicts comma (complete w spaces)") {
    assertPredictions(
      """{ "x": 0  }""",
      9,
      Seq(",", "}")
    )
  }

  test("object multiple attributes") {
    assertPredictions(
      """{ "x": 1, "y":""",
      14,
      allValues
    )
  }

  test("object multiple attributes ends comma") {
    assertPredictions(
      """{ "x": 1,""",
      9,
      Seq("\"\"")
    )
  }

  test("array open") {
    assertPredictions(
      "[",
      1,
      "]" :: allValues
    )
  }

  test("array closed") {
    assertPredictions(
      "[]",
      1,
      "]" :: allValues
    )
  }

  test("array with values") {
    assertPredictions(
      "[1,2 ",
      5,
      List(",", "]")
    )
  }

  test("array with values end comma") {
    assertPredictions(
      "[1,2,",
      5,
      allValues
    )
  }

}
