package diesel.json

import org.typelevel.jawn
import diesel.json.Ast.Position
import scala.util.Failure
import scala.util.Success

object JsonParser {

  private object Facade extends jawn.Facade[Ast.Value] {

    override def singleContext(index: Int): jawn.FContext[Ast.Value] = new jawn.FContext[Ast.Value]() {
      private var current: Option[Ast.Value]              = None
      override def add(s: CharSequence, index: Int): Unit = {
        val str = s.toString()
        current = Some(Ast.Str(Position(index, s.length() + 2), str))
      }
      override def add(v: Ast.Value, index: Int): Unit    =
        current = Some(v)
      override def finish(index: Int): Ast.Value          =
        current.getOrElse(throw new IllegalStateException("missing single value"))
      override def isObj: Boolean                         = false
    }

    override def arrayContext(startIndex: Int): jawn.FContext[Ast.Value] = new jawn.FContext[Ast.Value]() {
      private var vs: Seq[Ast.Value] = Seq.empty

      override def add(s: CharSequence, index: Int): Unit =
        vs = vs :+ Ast.Str(Position(index, s.length() + 2), s.toString())
      override def add(v: Ast.Value, index: Int): Unit    =
        vs = vs :+ v
      override def finish(index: Int): Ast.Value          =
        Ast.Array(Position(startIndex, index - startIndex + 1), vs)

      override def isObj: Boolean = false
    }

    override def objectContext(startIndex: Int): jawn.FContext[Ast.Value] = new jawn.FContext[Ast.Value]() {
      private var currentKey: Option[Ast.AttrName]        = None
      private var attributes: Seq[Ast.Attribute]          = Seq()
      override def add(s: CharSequence, index: Int): Unit =
        if (currentKey.isEmpty) {
          currentKey = Some(Ast.AttrName(Position(index, s.length() + 2), s.toString()))
        } else {
          val an = currentKey.get
          attributes = attributes :+ Ast.Attribute(
            Position(an.position.offset, (index + s.length()) - an.position.offset + 2),
            name = an,
            value = Ast.Str(Position(index, s.length() + 2), s.toString())
          )
          currentKey = None
        }
      override def add(v: Ast.Value, index: Int): Unit    =
        if (currentKey.isDefined) {
          val an = currentKey.get
          attributes = attributes :+ Ast.Attribute(
            Position(an.position.offset, (v.position.offset + v.position.length) - an.position.offset),
            an,
            v
          )
          currentKey = None
        }
      override def finish(index: Int): Ast.Value          =
        Ast.Object(Position(startIndex, index - startIndex + 1), attributes)
      override def isObj: Boolean                         = true
    }

    override def jnull(index: Int): Ast.Value = Ast.Null(Position(index, "null".length))

    override def jfalse(index: Int): Ast.Value = Ast.Bool(Position(index, "false".length), false)

    override def jtrue(index: Int): Ast.Value = Ast.Bool(Position(index, "true".length), true)

    override def jnum(s: CharSequence, decIndex: Int, expIndex: Int, index: Int): Ast.Value =
      Ast.Number(Position(index, s.length()), s.toString)

    override def jstring(s: CharSequence, index: Int): Ast.Value = Ast.Str(Position(index, s.length()), s.toString)

  }

  sealed trait JsonParseResult
  case class JPRError(message: String)    extends JsonParseResult
  case class JPRSuccess(value: Ast.Value) extends JsonParseResult

  def parse(text: String): JsonParseResult = jawn.Parser.parseFromString(text)(Facade) match {
    case Failure(exception) =>
      JPRError(exception.getMessage())
    case Success(value)     =>
      JPRSuccess(value)
  }
}
