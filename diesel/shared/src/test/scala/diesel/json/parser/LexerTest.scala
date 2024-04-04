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

package diesel.json.parser

import munit.FunSuite
import diesel.json.parser.Lexer._

class LexerTest extends FunSuite {

  private def assertOne(input: String, expected: Token): Unit = {
    val tokens = lexerToSeq(Lexer(input))
    assertEquals(tokens, Seq(expected))
  }

  private def lexerToSeq(lexer: Lexer): Seq[Token] = {
    lexer.next() match {
      case Some(t) =>
        t.tokenType match {
          case Invalid =>
            Seq.empty
          case _       =>
            t +: lexerToSeq(lexer)
        }
      case None    =>
        Seq.empty
    }
  }

  private def tokenize(input: String): Seq[Token] =
    lexerToSeq(Lexer(input))

  test("zero") {
    assertOne("0", Token(0, 1, NumberLiteral))
  }

  test("int literal") {
    assertOne("123", Token(0, 3, NumberLiteral))
  }

  test("bool literal") {
    assertOne("true", Token(0, 4, BooleanLiteral))
  }

  test("string literal") {
    assertOne("\"foo\"", Token(0, 5, StringLiteral))
  }

  test("null literal") {
    assertOne("null", Token(0, 4, NullLiteral))
  }

  test("empty array") {
    assertEquals(
      tokenize("[]"),
      Seq(
        Token(0, 1, OpenArray),
        Token(1, 1, CloseArray)
      )
    )
  }

  test("array") {
    assertEquals(
      tokenize("""[1,"foo",true]"""),
      Seq(
        Token(0, 1, OpenArray),
        Token(1, 1, NumberLiteral),
        Token(2, 1, Comma),
        Token(3, 5, StringLiteral),
        Token(8, 1, Comma),
        Token(9, 4, BooleanLiteral),
        Token(13, 1, CloseArray)
      )
    )
  }

  test("array 2") {
    assertEquals(
      tokenize("[true,]"),
      Seq(
        Token(0, 1, OpenArray),
        Token(1, 4, BooleanLiteral),
        Token(5, 1, Comma),
        Token(6, 1, CloseArray)
      )
    )
  }

  test("object") {
    assertEquals(
      tokenize("{}"),
      Seq(
        Token(0, 1, OpenObject),
        Token(1, 1, CloseObject)
      )
    )
  }

  test("truefalse") {
    assertEquals(
      tokenize("truefalse"),
      Seq(
        Token(0, 4, BooleanLiteral),
        Token(4, 5, BooleanLiteral)
      )
    )
  }

  test("num true (lookahead)") {
    assertEquals(
      tokenize("1,2"),
      Seq(
        Token(0, 1, NumberLiteral),
        Token(1, 1, Comma),
        Token(2, 1, NumberLiteral)
      )
    )
  }

}
