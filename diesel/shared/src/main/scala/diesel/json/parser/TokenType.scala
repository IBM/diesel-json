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
