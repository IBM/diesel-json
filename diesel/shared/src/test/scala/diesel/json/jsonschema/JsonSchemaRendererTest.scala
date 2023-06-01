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

import diesel.json.jsonschema.Schema2020_12.Renderer
import munit.FunSuite

class JsonSchemaRendererTest extends FunSuite {

  test("get renderer 1") {
    val schemaValue = Util.parseJson(
      """{
        | "type": "string",
        | "renderer": "Yalla"
        |}""".stripMargin
    ).asAstObject.get
    val s           = Util.parseSchemaValue(schemaValue)
    val value       = Util.parseJson(""""yolo"""")
    val res         = s.validate(value)
    val resAtPath   = res.flatten.find(r => r.path == JPath.empty).get
    assertEquals(resAtPath.renderer, Some(Renderer("Yalla", schemaValue)))
  }

//  private def clearRendererPos(renderer: Renderer): Renderer =
//    renderer.copy(value = renderer.value.map(_.clearPosition))

  test("get renderer 2") {
    val schemaValue = Util.parseJson(
      """{
        | "type": "string",
        | "renderer": {
        |   "key": "Yalla"
        | }
        |}""".stripMargin
    ).asAstObject.get
    val s           = Util.parseSchemaValue(schemaValue)
    val value       = Util.parseJson(""""yolo"""")
    val res         = s.validate(value)
    val resAtPath   = res.flatten.find(r => r.path == JPath.empty).get
    assertEquals(
      resAtPath.renderer,
      Some(
        Renderer("Yalla", schemaValue)
      )
    )
  }

  test("get renderer 3") {
    val schemaValue = Util.parseJson(
      """{
        | "type": "string",
        | "renderer": {
        |   "key": "Yalla",
        |   "foo": 123
        | }
        |}""".stripMargin
    ).asAstObject.get
    val s           = Util.parseSchemaValue(schemaValue)
    val value       = Util.parseJson(""""yolo"""")
    val res         = s.validate(value)
    val resAtPath   = res.flatten.find(r => r.path == JPath.empty).get
    assertEquals(
      resAtPath.renderer,
      Some(
        Renderer("Yalla", schemaValue)
      )
    )
  }

}
