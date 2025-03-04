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

import diesel.Context
import diesel.json.jsonschema.JPath

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.Try

object Ast {

  case class Position(offset: Int, length: Int)

  object Position {
    def apply(ctx: Context): Position = Position(ctx.offset, ctx.length)
    val zero: Position                = Position(0, 0)
  }

  sealed trait Node {
    val position: Position

    def find(p: Ast.Node => Boolean): Seq[Node] =
      if (p(this)) {
        Seq(this)
      } else {
        Seq.empty
      }
  }

  sealed trait Value extends Node {

    private def convert[T: ClassTag]: Option[T] =
      this match {
        case el: T =>
          Some(el)
        case _     =>
          None
      }

    def asAstStr: Option[Str]           = convert[Str]
    def asAstNumber: Option[Ast.Number] = convert[Ast.Number]
    def asAstBoolean: Option[Ast.Bool]  = convert[Ast.Bool]
    def asAstArray: Option[Ast.Array]   = convert[Ast.Array]
    def asAstObject: Option[Ast.Object] = convert[Ast.Object]
    def asAstNull: Option[Ast.Null]     = convert[Ast.Null]

    def asString: Option[String] = convert[Str].map(_.v)
    def asInt: Option[Int]       = asAstNumber.flatMap(v => Try(v.v.toInt).toOption)
    def asDouble: Option[Double] = asAstNumber.flatMap(v => Try(v.v.toDouble).toOption)

    final def equalsIgnorePos(that: Value): Boolean =
      this.clearPosition == that.clearPosition

    def stringify: String

    override def toString: String = stringify

    def clearPosition: Ast.Value

  }

  case class Null(position: Position) extends Value {
    override def clearPosition: Null = Null(Position.zero)
    override def stringify: String   = "null"
  }

  case class Object(position: Position, attributes: Seq[Attribute]) extends Value {
    override def clearPosition: Value     =
      this.copy(position = Position.zero, attributes = this.attributes.map(_.clearPosition))
    def attr(name: String): Option[Value] = attributes.find(_.name.s == name).map(_.value)

    override def find(p: Node => Boolean): Seq[Node] = {
      val attrValues = attributes
        .map(_.value)
        .flatMap(_.find(p))
      super.find(p) ++ attrValues
    }

    override def stringify: String = s"{${attributes.map(_.stringify).mkString(",")}}"
  }

  case class Array(position: Position, elems: Seq[Value]) extends Value {
    override def clearPosition: Value                = this.copy(position = Position.zero)
    def elemAt(index: Int): Option[Value]            = elems.lift(index)
    override def find(p: Node => Boolean): Seq[Node] = {
      super.find(p) ++ elems.flatMap(_.find(p))
    }

    override def stringify: String = s"[${elems.map(_.stringify).mkString(",")}]"
  }

  case class Number(position: Position, v: String) extends Value {
    override def clearPosition: Value = this.copy(position = Position.zero)
    override def stringify: String    = v
  }

  case class Str(position: Position, v: String) extends Value {
    override def clearPosition: Value = this.copy(position = Position.zero)
    override def stringify: String    = "\"" + v + "\""
  }

  case class Bool(position: Position, v: Boolean) extends Value {
    override def clearPosition: Value = this.copy(position = Position.zero)
    override def stringify: String    = v.toString
  }

  case class Attribute(position: Position, name: AttrName, value: Value) {
    def clearPosition: Attribute                  =
      this.copy(position = Position.zero, name = name.clearPosition, value = value.clearPosition)
    def equalsIgnorePos(that: Attribute): Boolean =
      name.s == that.name.s && value.equalsIgnorePos(that.value)
    def stringify: String                         = "\"" + name.s + "\":" + value.stringify
  }

  case class AttrName(position: Position, s: String) {
    def clearPosition: AttrName = this.copy(position = Position.zero)
  }

  object Builder {

    val nullValue: Ast.Null                                              = Ast.Null(Position.zero)
    def obj(attributes: Seq[Ast.Attribute]): Ast.Object                  =
      Ast.Object(Position.zero, attributes)
    def obj(attr: Ast.Attribute, attributes: Ast.Attribute*): Ast.Object = {
      obj(Seq(attr) ++ attributes)
    }
    def attr(name: String, value: Ast.Value): Ast.Attribute              =
      Ast.Attribute(
        Position.zero,
        Ast.AttrName(Position.zero, name),
        value
      )
    def array(items: Seq[Ast.Value]): Ast.Array                          =
      Ast.Array(Position.zero, items)
    def array(item: Ast.Value, items: Ast.Value*): Ast.Array             =
      array(Seq(item) ++ items)
    def num(value: String): Ast.Number                                   =
      Ast.Number(Position.zero, value)
    def bool(value: Boolean): Ast.Bool                                   =
      Ast.Bool(Position.zero, value)
    def str(value: String): Ast.Str                                      =
      Ast.Str(Position.zero, value)

    object Implicits {
      implicit def boolToBool(x: Boolean): Ast.Bool = bool(x)

      implicit def strToStr(x: String): Ast.Str = str(x)

      implicit def tupleToAttr(t: (String, Ast.Value)): Ast.Attribute = attr(t._1, t._2)
    }
  }

  import Builder._

  object Constants {
    val astBool: Ast.Bool     = bool(true)
    val astObject: Ast.Object = obj(Seq.empty)
    val astStr: Ast.Str       = str("")
    val astNumber: Ast.Number = num("0")
    val astArray: Ast.Array   = array(Seq.empty)
    val astNull: Ast.Null     = nullValue

    val all: Seq[Ast.Value] = Seq(
      astNull,
      astBool,
      astStr,
      astNumber,
      astObject,
      astArray
    )
  }

  object Value {

    def map(root: Ast.Value, path: JPath)(f: Ast.Value => Ast.Value): Ast.Value = {
      if (path.isEmpty) {
        f(root)
      } else {
        path.elems.toList match {
          case head :: tail =>
            root match {
              case x: Object =>
                x.copy(attributes =
                  x.attributes.map { attr =>
                    if (attr.name.s == head) {
                      // advance !
                      attr.copy(value =
                        map(attr.value, JPath(tail))(f)
                      )
                    } else {
                      attr
                    }
                  }
                )
              case x: Array  =>
                Try(head.toInt).toOption
                  .map { pathIndex =>
                    x.copy(elems =
                      x.elems.zipWithIndex.map { case (elemValue, elemIndex) =>
                        if (elemIndex == pathIndex) {
                          map(elemValue, JPath(tail))(f)
                        } else {
                          elemValue
                        }
                      }
                    )
                  }
                  .getOrElse(x)
              case _         =>
                root
            }
          case Nil          =>
            root
        }
      }
    }

  }

}
