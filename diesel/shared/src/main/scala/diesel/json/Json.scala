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

import diesel.Dsl.{Axiom, Concept, Instance, Syntax}
import diesel._
import diesel.json.Ast._
import diesel.json.Styles._

import scala.util.Try

object Json extends Dsl {

  sealed trait JsonParseResult
  case class JPRError(message: String)                       extends JsonParseResult
  case class JPRSuccess(tree: GenericTree, value: Ast.Value) extends JsonParseResult

  def parse(text: String): JsonParseResult = {
    val result = AstHelpers.parse(this, text)
    if (result.success) {
      val navigator = Navigator(result)
      if (!navigator.hasNext) {
        JPRError("No AST found")
      } else {
        val ast = navigator.next()
        if (navigator.hasNext) {
          JPRError("Too many ASTs")
        } else {
          ast.root.value match {
            case v: Ast.Value =>
              JPRSuccess(ast, v)
            case _            =>
              JPRError("AST root is not a Value")
          }
        }
      }
    } else {
      JPRError("result error " + result.reportErrors().mkString(", "))
    }
  }

  val value: Concept[Value] = concept

  val iNull: Instance[Value] = instance(value)("null") map { c =>
    Null(Position(c))
  }

  val bool: Concept[Bool] = concept

  val bTrue: Instance[Bool] = instance(bool)("true") map { c =>
    Bool(Position(c), v = true)
  }

  val bFalse: Instance[Bool] = instance(bool)("false") map { c =>
    Bool(Position(c), v = false)
  }

  val bool_v: Syntax[Value] = syntax(value)(
    bool map {
      case (c, b) =>
        c.setStyle(JBool)
        b
    }
  )

  val numberDefaultValue: Number = Number(Position(0, 0), 0)

  val number: Concept[Number] = concept(
    "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?".r,
    numberDefaultValue,
    Some(value)
  ).valueToString((n: Number) => "%1.0f" format n.v) map { (c, t) =>
    c.setStyle(JNumber)
    Number(Position(c), Try(t.text.toDouble).getOrElse(0))
  }

  val dblQuotedString: Concept[String] = concept(
    "\"([^\"\\\\]|\\\\.)*\"".r,
    "\"\""
  ) map { (_, t) => t.text.drop(1).dropRight(1) }

  val str: Syntax[Str] = syntax(dblQuotedString map { (c, s) =>
    c.setStyle(JString)
    Str(Position(c), s)
  })

  val str_v: Syntax[Value] = syntax(value)(str map { (_, s) => s })

  val arrayValue: Syntax[Value] = syntax(value)

  val arrayValues: Syntax[Seq[Value]] = syntax(
    (arrayValue ~ ("," ~ arrayValue).rep(true)) map {
      case (_, (v, vs)) =>
        Seq(v) ++ vs.map(_._2)
    }
  )

  val array: Syntax[Array] = syntax(
    ("[" ~ arrayValues.? ~ "]") map {
      case (c, (_, Some(values), _)) =>
        Array(Position(c), values)
      case (c, (_, None, _))         =>
        Array(Position(c), Seq.empty)
    }
  )

  val array_v: Syntax[Value] = syntax(value)(array map { (_, a) => a })

  val attrConcept: Concept[AttrName] = concept[AttrName]

  val attrName: Syntax[AttrName] = syntax(attrConcept)(
    dblQuotedString map { (c, s) =>
      c.setStyle(JAttr)
      AttrName(Position(c), s)
    }
  )

  val attrValue: Syntax[Value] = syntax(value)

  val attr: Syntax[Attribute] = syntax(
    attrName ~ ":" ~ attrValue map {
      case (c, (name, _, v)) =>
        Attribute(Position(c), name, v)
    }
  )

  val attrs: Syntax[Seq[Attribute]] = syntax(
    attr ~ ("," ~ attr).rep(true) map {
      case (_, (a, as)) =>
        Seq(a) ++ as.map(_._2)
    }
  )

  val obj: Syntax[Object] = syntax(
    "{" ~ attrs.? ~ "}" map {
      case (c, (_, x, _)) =>
        Object(Position(c), x.getOrElse(Seq.empty))
    }
  )

  val obj_v: Syntax[Value] = syntax(value)(obj map { (_, o) => o })

  val sAxiom: Syntax[Value] = syntax(value)

  val ax: Axiom[Value] = axiom(sAxiom) map { (c, v) =>
    JsonBuiltInValidator.validate(v).foreach(m => c.addMarkers(m))
    v
  }

}
