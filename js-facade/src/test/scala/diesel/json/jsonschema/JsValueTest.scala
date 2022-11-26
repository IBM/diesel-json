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
import munit.FunSuite

import scala.scalajs.js

class JsValueTest extends FunSuite {

  private def doTest(v: Any)(check: Ast.Value => Unit): Unit = {
//    println("v=" + v)
    val x = JsValue.toValue(v)
//    println("x=" + x)
    check(x)
  }

  test("int") {
    doTest(123) { v =>
      assert(v.asAstNumber.map(_.v).get == 123)
    }
  }

  test("obj") {
    val o = js.Dynamic.literal(
      "yalla" -> 123
    )
    doTest(o) { v =>
      val yallaValue = v.asAstObject.flatMap(_.attr("yalla")).flatMap(_.asAstNumber)
      assert(yallaValue.exists(_.v == 123))
    }
  }

  test("array") {
    val a = js.Array("foo", 123)
    doTest(a) { v =>
      val astArr = v.asAstArray
      val elem0  = astArr.flatMap(_.elemAt(0)).flatMap(_.asString)
      assert(elem0.get == "foo")
      val elem1  = astArr.flatMap(_.elemAt(1)).flatMap(_.asAstNumber).map(_.v)
      assert(elem1.get == 123)
    }
  }

  test("complex") {
    val o = js.Dynamic.literal(
      "foo"  -> "bar",
      "baz"  -> js.Array(true, "false"),
      "blah" -> js.Dynamic.literal(
        "funk" -> "soul"
      )
    )
    doTest(o) { v =>
      val obj     = v.asAstObject
      val fooVal  = obj.flatMap(_.attr("foo").flatMap(_.asString))
      assert(fooVal.get == "bar")
      val bazVal  = obj.flatMap(_.attr("baz").flatMap(_.asAstArray))
      val bazVal0 = bazVal.flatMap(_.elemAt(0)).flatMap(_.asAstBoolean).map(_.v)
      assert(bazVal0.get)
      val bazVal1 = bazVal.flatMap(_.elemAt(1)).flatMap(_.asString)
      assert(bazVal1.get == "false")
      val blahVal = obj.flatMap(_.attr("blah")).flatMap(_.asAstObject)
      val funkVal = blahVal.flatMap(_.attr("funk")).flatMap(_.asString)
      assert(funkVal.get == "soul")
    }
  }

}
