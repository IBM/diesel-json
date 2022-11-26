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
import diesel.json.jsonschema.Schema2020_12._
import diesel.json.jsonschema.Util.parseSchema
import munit.FunSuite

class JsonSchemaTest extends FunSuite {

  private def assertSchema(text: String, schema: JsonSchema): Unit =
    parseSchema(text) == schema

  test("true") {
    assertSchema("true", SchemaBool(true))
  }

  test("false") {
    assertSchema("false", SchemaBool(false))
  }

  test("empty") {
    val schema = parseSchema("{}")
    schema.asInstanceOf[SchemaObject].types.isEmpty
  }

  test("id") {
    val s = parseSchema("""{ "$id": "yalla" }""")
    assert(
      s.asInstanceOf[SchemaObject].id.contains("yalla")
    )
  }

  test("long") {
    val s = parseSchema(Examples.Long)
    s.asInstanceOf[SchemaObject].types match {
      case (t1: TInteger) :: TNull :: Nil =>
        assert(t1.format.contains("int64"))
      case _                              =>
        fail("unexpected")
    }
  }

  test("string") {
    val s = parseSchema(Examples.String)
    s.asInstanceOf[SchemaObject].types match {
      case t1 :: TNull :: Nil =>
        assert(t1.isInstanceOf[TString])
      case _                  =>
        fail("unexpected")
    }
  }

  test("enum array") {
    val s = parseSchema(Examples.EnumArray)
    s.asInstanceOf[SchemaObject].types match {
      case (a: TArray) :: TNull :: Nil =>
        a.items match {
          case Some(ListValidation(itemSchema)) =>
            val iso = itemSchema.asInstanceOf[SchemaObject]
            iso.types match {
              case (_: TString) :: TNull :: Nil =>
                val enums = iso.enum1
                assert(enums.size == 2)
                assert(enums.toArray.apply(0).asString.contains("FOO"))
                assert(enums.toArray.apply(1).asString.contains("BAR"))
              case _                            =>
                fail("unexpected")
            }
          case _                                =>
            fail("unexpected")
        }
      case _                           =>
        fail("unexpected")
    }
  }

  test(" Polymorphism (lions and elephants are animal)") {
    val schema         = parseSchema(Examples.Polymorphism)
    val so             = schema.asInstanceOf[SchemaObject]
    assert(so.allOf.size == 2)
    val allOf0         = so.allOf.head.asInstanceOf[SchemaObject]
    val ite            = allOf0.ifThenElse.get
    val iteIf          = ite.`if`.asInstanceOf[SchemaObject]
    assert(iteIf.types.size == 1)
    val iteIfSchema    = iteIf.types.head.asInstanceOf[TObject]
    assert(iteIfSchema.properties.size == 1)
    val whatPropSchema = iteIfSchema.properties("what").asInstanceOf[SchemaObject]
    assert(whatPropSchema.types.size == 1)
    val t0             = whatPropSchema.types.head
    assert(t0.isInstanceOf[TString])
    assert(whatPropSchema.const.exists(c => {
      c.asInstanceOf[Ast.Str].v == "schema.animal.Lion"
    }))
  }

  test("Cycle") {
    val schema = parseSchema(Examples.Cycle)
    val so     = schema.asInstanceOf[SchemaObject]
    so.types match {
      case (to: TObject) :: Nil =>
        assert(to.properties("next").isInstanceOf[SchemaRef])
      case _                    =>
        fail("unexpected")
    }
  }

  test("Unwrapping") {
    val schema = parseSchema(Examples.Unwrapping)
    val so     = schema.asInstanceOf[SchemaObject]
    so.types match {
      case (to: TObject) :: Nil =>
        val nameFirst = to.properties("name.first").asInstanceOf[SchemaObject]
        nameFirst.types match {
          case t1 :: t2 :: Nil =>
            assert(t1.isInstanceOf[TString])
          case _               =>
            fail("unexpected")
        }
      case _                    =>
        fail("unexpected")
    }
  }

  private val schemaDate: String = """{
                                     |  "type": "string",
                                     |  "format": "date"
                                     |}""".stripMargin

  test("format") {
    val schema = parseSchema(schemaDate)
    val so     = schema.asInstanceOf[SchemaObject]
    so.types match {
      case (to: TString) :: Nil =>
        assert(to.format.contains(FDate))
      case _                    =>
        fail("unexpected")
    }
  }

}
