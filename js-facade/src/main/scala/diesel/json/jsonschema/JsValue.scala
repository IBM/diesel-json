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
import diesel.json.Ast.Position

import scala.scalajs.js

object JsValue {

  def toValue(v: Any): Ast.Value = v match {
    case x: String      =>
      Ast.Str(Position.zero, x)
    case x: Double      =>
      Ast.Number(Position.zero, x)
    case x: Int         =>
      Ast.Number(Position.zero, x)
    case x: Boolean     =>
      Ast.Bool(Position.zero, x)
    case x: js.Array[_] =>
      Ast.Array(Position.zero, x.map(toValue).toSeq)
    case x: js.Object   =>
      val dict                           = x.asInstanceOf[js.Dictionary[_]]
      val attributes: Seq[Ast.Attribute] =
        js.Object.keys(x)
          .map { key =>
            val jsVal = dict(key)
            Ast.Attribute(Position.zero, Ast.AttrName(Position.zero, key), toValue(jsVal))
          }
          .toSeq
      Ast.Object(Position.zero, attributes)
    case _              =>
      Ast.Null(Position.zero)
  }

  def fromValue(v: Ast.Value): Any = {
    v match {
      case Ast.Null(_)               =>
        null
      case Ast.Object(_, attributes) =>
        js.Dictionary(
          attributes.map(a => a.name.s -> fromValue(a.value)): _*
        )
      case Ast.Array(_, elems)       =>
        js.Array(elems.map(e => fromValue(e)): _*)
      case Ast.Number(_, v)          =>
        v
      case Ast.Str(_, v)             =>
        v
      case Ast.Bool(_, v)            =>
        v
    }
  }

}
