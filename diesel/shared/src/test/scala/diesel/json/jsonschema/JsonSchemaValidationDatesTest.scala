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

import diesel.json.jsonschema.Util.parseSchema
import diesel.json.jsonschema.JPath.Implicits._
import diesel.json.jsonschema.Schema2020_12.{FDate, FDateTime, FTime}
import munit.FunSuite

class JsonSchemaValidationDatesTest extends FunSuite {

  private def assertErrors(schema: String)(json: String, expected: Any): Unit =
    Util.assertErrors(parseSchema(schema))(json) { (_, e) => assert(e == expected) }

  private def assertNoErrors(schema: String)(json: String): Unit =
    Util.assertErrors(parseSchema(schema))(json) { (_, e) => assert(e.isEmpty) }

  private val schemaDate: String = """{
                                     |  "type": "string",
                                     |  "format": "date"
                                     |}""".stripMargin

  test("date ok") {
    assertNoErrors(schemaDate)("\"2018-11-13\"")
  }

  test("date error") {
    assertErrors(schemaDate)("\"not a date\"", Seq(StringFormatError("/", FDate)))
  }

  private val schemaTime: String = """{
                                     |  "type": "string",
                                     |  "format": "time"
                                     |}""".stripMargin

  test("time with offset") {
    assertNoErrors(schemaTime)("\"20:20:39+00:00\"")
  }

  test("time zulu") {
    assertNoErrors(schemaTime)("\"20:20:39Z\"")
  }

  test("local time") {
    assertNoErrors(schemaTime)("\"20:20:39\"")
  }

  test("time error") {
    assertErrors(schemaTime)("\"not a time\"", Seq(StringFormatError("/", FTime)))
    assertErrors(schemaTime)("\"20:20:39XYXY\"", Seq(StringFormatError("/", FTime)))
  }

  private val schemaDateTime: String = """{
                                         |  "type": "string",
                                         |  "format": "date-time"
                                         |}""".stripMargin

  test("date-time ok") {
    assertNoErrors(schemaDateTime)("\"2018-11-13T20:20:39+00:00\"")
  }

  test("date-time error") {
    assertErrors(schemaDateTime)("\"not a time\"", Seq(StringFormatError("/", FDateTime)))
    assertErrors(schemaDateTime)("\"2018-11-13T20:20:39\"", Seq(StringFormatError("/", FDateTime)))
    assertErrors(schemaDateTime)("\"2018-11-13\"", Seq(StringFormatError("/", FDateTime)))
  }

}
