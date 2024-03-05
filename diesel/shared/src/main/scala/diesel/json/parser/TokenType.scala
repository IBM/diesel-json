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
