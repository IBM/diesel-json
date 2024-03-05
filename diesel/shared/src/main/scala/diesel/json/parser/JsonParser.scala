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

import scala.collection.mutable.ArrayBuffer

object JsonParser {

  sealed trait Value
  case object VNull                               extends Value
  case class VString(v: String)                   extends Value
  case class VNumber(v: String)                   extends Value
  case class VBool(v: Boolean)                    extends Value
  case class VArray(vs: Seq[Value])               extends Value
  case class VObject(attrs: Seq[(String, Value)]) extends Value

  def parse(input: String): Either[String, Value] = new JsonParser(input, Lexer(input)).value()

}

class JsonParser(private val input: String, private val lexer: Lexer) {

  import Lexer._
  import JsonParser._

  var lookahead: NextToken = lexer.next()

  private def moveToNext(): Unit = {
    lookahead = lexer.next()
  }

  private def failEos(offset: Int)                                                    = s"Unexpected eos at offset $offset"
  private def failUnexpectedToken(offset: Int, found: TokenType, expected: TokenType) =
    s"Unexpected token at $offset, expected $expected, found $found"
  private def failInvalidToken(offset: Int): String                                   = s"Invalid token at offset $offset"

  private def matching(t: TokenType): Option[String] = {
    lookahead match {
      case Eos(offset)               =>
        Some(failEos(offset))
      case InvalidToken(offset)      =>
        Some(failInvalidToken(offset))
      case ValidToken(offset, token) =>
        if (token.tokenType == t) {
          lookahead = lexer.next()
          None
        } else {
          Some(failUnexpectedToken(offset, token.tokenType, t))
        }
    }
  }

  private def tokenText(token: Token): String = {
    if (token.offset + token.length > input.length()) {
      throw new RuntimeException("out of bounds")
    } else {
      input.substring(token.offset, token.offset + token.length)
    }
  }

  def value(): Either[String, Value] = lookahead match {
    case Eos(offset)               =>
      Left(failEos(offset))
    case InvalidToken(offset)      =>
      Left(failInvalidToken(offset))
    case ValidToken(offset, token) =>
      token.tokenType match {
        case NullLiteral    =>
          moveToNext()
          Right(VNull)
        case NumberLiteral  =>
          moveToNext()
          Right(VNumber(tokenText(token)))
        case StringLiteral  =>
          moveToNext()
          Right(VString(tokenText(token)))
        case BooleanLiteral =>
          moveToNext()
          Right(VBool(tokenText(token).toBoolean))
        case OpenArray      =>
          array()
        case OpenObject     =>
          ???
        case _              =>
          Left(s"Unexpected token '${tokenText(token)}' at $offset")
      }
  }

  def array(): Either[String, Value] =
    matching(OpenArray) match {
      case None =>
        var closed                    = false
        val values                    = ArrayBuffer[Value]()
        var err: Option[String]       = None
        var lastCommaPos: Option[Int] = None
        while (!closed && err.isEmpty) {
          lookahead match {
            case Eos(offset)                                 =>
              err = Some(failEos(offset))
            case InvalidToken(offset)                        =>
              err = Some(failInvalidToken(offset))
            case ValidToken(offset, Token(_, _, CloseArray)) =>
              lastCommaPos match {
                case Some(offset) =>
                  err = Some(s"Trailing comma at offset $offset")
                case None         =>
                  closed = true
                  moveToNext()
              }
            case ValidToken(offset, Token(_, _, Comma))      =>
              lastCommaPos match {
                case None        =>
                  lastCommaPos = Some(offset)
                  moveToNext()
                case Some(value) =>
                  err = Some(s"Unexpected comma at offset $offset")
              }
            case _                                           =>
              value() match {
                case Left(value)  =>
                  err = Some(value)
                case Right(value) =>
                  values.append(value)
                  lastCommaPos = None
              }
          }
        }
        err
          .map(e => Left(e))
          .getOrElse(Right(VArray(values.toSeq)))

      case Some(value) =>
        Left(value)
    }

  // def value(lexer: Lexer): Either[String, Value] = {
  //     val t = l.next()
  //     t match {
  //         case Eos =>
  //             Left("empty string")
  //         case InvalidToken(index) =>
  //             Left("invalid token")
  //         case ValidToken(token) =>
  //             token.tokenType match {
  //                 case OpenArray =>

  //                 case NumberLiteral =>
  //                 case Comma =>
  //                 case StringLiteral =>
  //                 case SemiColon =>
  //                 case NullLiteral =>
  //                 case OpenObject =>
  //                 case CloseArray =>
  //                 case BooleanLiteral =>
  //                 case CloseObject =>
  //             }
  //     }
  // }

}
