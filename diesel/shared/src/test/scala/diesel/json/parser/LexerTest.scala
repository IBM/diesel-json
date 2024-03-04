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
      case Eos               =>
        Seq.empty
      case InvalidToken(_)   =>
        Seq.empty
      case ValidToken(token) =>
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

}
