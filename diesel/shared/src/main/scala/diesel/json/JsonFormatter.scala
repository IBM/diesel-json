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

import diesel.json.Ast.{Attribute, Object, Value}

object JsonFormatter {

  def format(ast: Value, spaces: Int = 2, deepness: Int = 1): String = {
    val attrSpacing = " " * (spaces * deepness)
    val sepSpacing  = " " * (spaces * (deepness - 1))
    ast match {
      case Object(_, attributes) =>
        val attrStr: String = attributes.map((attr: Attribute) => {
          val child = format(attr.value, spaces, deepness + 1)
          s""""${attr.name.s}": ${child}"""
        }).mkString(s",\n$attrSpacing")
        s"""{
           |$attrSpacing${attrStr}
           |$sepSpacing}""".stripMargin
      case Ast.Array(_, elems)   =>
        val child = elems.map(elem => format(elem, spaces, deepness + 1))
        s"""[
           |$attrSpacing${child.mkString(s",\n$attrSpacing")}
           |$sepSpacing]""".stripMargin
      case Ast.Number(_, v)      => v.toString
      case Ast.Str(_, v)         => s""""${v}""""
      case Ast.Bool(_, v)        => v.toString
      case Ast.Null(_)           => "null"
    }
  }

}
