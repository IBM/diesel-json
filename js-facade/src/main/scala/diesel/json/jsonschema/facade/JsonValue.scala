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

package diesel.json.jsonschema.facade

import diesel.json.Ast

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
case class JsonValue(astValue: Ast.Value)

object JsonValue {
  def astToJsonValue(v: Ast.Value): js.Any = v match {
    case Ast.Null(_)               =>
      js.Dynamic.literal("tag" -> "jv-null")
    case Ast.Object(_, attributes) =>
      js.Dynamic.literal(
        "tag"        -> "jv-object",
        "properties" -> attributes.map(a =>
          js.Dynamic.literal("name" -> a.name.s, "value" -> astToJsonValue(a.value))
        ).toJSArray
      )
    case Ast.Array(_, elems)       =>
      js.Dynamic.literal(
        "tag"   -> "jv-array",
        "elems" -> elems.map(astToJsonValue).toJSArray
      )
    case Ast.Number(_, v)          =>
      js.Dynamic.literal("tag" -> "jv-number", "value" -> v)
    case Ast.Str(_, v)             =>
      js.Dynamic.literal("tag" -> "jv-string", "value" -> v)
    case Ast.Bool(_, v)            =>
      js.Dynamic.literal("tag" -> "jv-boolean", "value" -> v)
  }

}
