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

import munit.FunSuite
import diesel.json.jsonschema.Schema2020_12.SchemaObject

class JsonSchemaDiscriminatorTest extends FunSuite {

  test("get discriminator") {
    val schemaValue = Util.parseJson(
      """{
        | "discriminator": "gni"
        |}""".stripMargin
    ).asAstObject.get
    val s           = Util.parseSchemaValue(schemaValue)
    val value       = Util.parseJson(""""{}"""")
    val res         = s.validate(value)
    assertEquals(res.getErrors, Seq())
    val resAtPath   = res.flatten.find(r => r.path == JPath.empty).get
    val d           = resAtPath.schema.asInstanceOf[SchemaObject].node
      .attr("discriminator")
      .flatMap(_.asAstStr)
      .map(_.v)
    assertEquals(d, Some("gni"))
  }

}
