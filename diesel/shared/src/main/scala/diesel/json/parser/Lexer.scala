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

import scala.io.Source

object Lexer {

  sealed trait TokenType
  case object StringLiteral  extends TokenType
  case object BooleanLiteral extends TokenType
  case object NullLiteral    extends TokenType
  case object NumberLiteral  extends TokenType
  case object OpenObject     extends TokenType
  case object CloseObject    extends TokenType
  case object OpenArray      extends TokenType
  case object CloseArray     extends TokenType
  case object SemiColon      extends TokenType
  case object Comma          extends TokenType
  case object Invalid        extends TokenType

  case class Token(offset: Int, length: Int, tokenType: TokenType)

  def apply(input: String): Lexer = new Lexer(Source.fromString(input))
}

class Lexer(input: Source) {

  private var curIndex: Int           = 0
  private var lookahead: Option[Char] = None

  import Lexer._

  private def eatWhitespaces(): (Option[Char], Int) = {
    var res: Option[Char] = None
    var nbNext            = 0
    while (input.hasNext && res.isEmpty) {
      val c = input.next()
      if (!c.isWhitespace) {
        res = Some(c)
      } else {
        nbNext += 1
      }
    }
    (res, nbNext)
  }

  private def scanStr(): Option[String] = {
    val buf    = new StringBuilder()
    var closed = false
    while (input.hasNext && !closed) {
      val c = input.next()
      if (c == '"') {
        // TODO check escaping
        closed = true
      } else {
        buf.append(c)
      }
    }
    if (closed) {
      Some(buf.toString())
    } else {
      None
    }
  }

  private def scanChars(s: String): Boolean = {
    var i         = 0
    var foundDiff = false
    while (input.hasNext && i < s.length && !foundDiff) {
      val nextChar     = input.next()
      val nextExpected = s.charAt(i)
      foundDiff = nextChar != nextExpected
      i += 1
    }
    !foundDiff
  }

  private val digits        = "123456789".toSet
  private val digitsAndZero = "0123456789".toSet

  private def scanUnsignedNum(offset: Int, firstChar: Char): Option[(Token, Option[Char])] = {
    val sb = new StringBuilder()

    if (firstChar == '0') {
      println("ùml")
    }

    if (digitsAndZero.contains(firstChar)) {
      sb.append(firstChar)
      var lookahead: Option[Char] = None
      var isDijit                 = true
      while (input.hasNext && isDijit) {
        val ch = input.next()
        if (digitsAndZero.contains(ch)) {
          sb.append(ch)
        } else {
          isDijit = false
          lookahead = Some(ch)
        }
      }
      if (sb.length() > 0) {
        Some((Token(offset, sb.length(), NumberLiteral), lookahead))
      } else {
        None
      }
    } else {
      Some((Token(offset, 1, Invalid), Some(firstChar)))
    }
  }

  def next(): Option[Token] = {
    // eat whitespaces
    var (curChar, nbSkipped) = eatWhitespaces();
    curIndex += nbSkipped
    curChar.flatMap {
      case '{'     =>
        val t = Some(Token(curIndex, 1, OpenObject))
        curIndex += 1
        t
      case '}'     =>
        val t = Some(Token(curIndex, 1, CloseObject))
        curIndex += 1
        t
      case '['     =>
        val t = Some(Token(curIndex, 1, OpenArray))
        curIndex += 1
        t
      case ']'     =>
        val t = Some(Token(curIndex, 1, CloseArray))
        curIndex += 1
        t
      case ','     =>
        val t = Some(Token(curIndex, 1, Comma))
        curIndex += 1
        t
      case ':'     =>
        val t = Some(Token(curIndex, 1, SemiColon))
        curIndex += 1
        t
      case '"'     =>
        scanStr().map { s =>
          val len = s.length()
          val t   = Token(curIndex, len + 2, StringLiteral)
          curIndex += s.length() + 1 // closing "
          t
        }
      case 't'     =>
        if (scanChars("rue")) {
          val t = Some(Token(curIndex, 4, BooleanLiteral))
          curIndex += 4
          t
        } else {
          None
        }
      case 'f'     =>
        if (scanChars("alse")) {
          val t = Some(Token(curIndex, 5, BooleanLiteral))
          curIndex += 5
          t
        } else {
          None
        }
      case 'n'     =>
        if (scanChars("ull")) {
          val t = Some(Token(curIndex, 4, NullLiteral))
          curIndex += 4
          t
        } else {
          None
        }
      case c: Char =>
        scanUnsignedNum(curIndex, c) match {
          case None           =>
            Some(Token(curIndex, 0, Invalid))
          case Some((tk, la)) =>
            lookahead = la
            Some(tk)
        }
    }
  }

}
