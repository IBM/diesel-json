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
import diesel.json.parser.Lexer.Eos
import diesel.json.parser.Lexer.InvalidToken
import diesel.json.parser.Lexer.ValidToken

class LexerTest extends FunSuite {

  private def assertOne(input: String, expected: Token): Unit = {
    val tokens = lexerToSeq(Lexer(input))
    assertEquals(tokens, Seq(expected))
  }

  private def lexerToSeq(lexer: Lexer): Seq[Token] = {
    lexer.next() match {
      case Eos(_)               =>
        Seq.empty
      case InvalidToken(_)      =>
        Seq.empty
      case ValidToken(_, token) =>
        Seq(token) ++ lexerToSeq(lexer)
    }
  }

  private def tokenize(input: String): Seq[Token] =
    lexerToSeq(Lexer(input))

  private def assertNumberRule(text: String, offset: Int, expected: Option[Token]) = {
    val t = Lexer.NumberRule.token(text, offset)
    assertEquals(t, expected)
  }

  test("number rule") {
    assertNumberRule("", 0, None)
    assertNumberRule("abc", 0, None)
    assertNumberRule("  ", 2, None)
    assertNumberRule("x1", 0, None)
    assertNumberRule(" 1", 0, None)
    assertNumberRule("  123", 2, Some(Token(2, 3, NumberLiteral)))
    assertNumberRule("1", 0, Some(Token(0, 1, NumberLiteral)))
    assertNumberRule("1   ", 0, Some(Token(0, 1, NumberLiteral)))
    assertNumberRule("123", 0, Some(Token(0, 3, NumberLiteral)))
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
      tokenize("[123,]"),
      Seq(
        Token(0, 1, OpenArray),
        Token(1, 3, NumberLiteral),
        Token(4, 1, Comma),
        Token(5, 1, CloseArray)
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

}
