package diesel.json.jsonschema.facade

import diesel.json.Ast

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object JsonValue {

//  sealed trait JsonValue {
//    val tag: String
//  }
//
//  @JSExportAll
//  case object JsonNull extends JsonValue {
//    override val tag = "jv-null";
//  }
//
//  @JSExportAll
//  case class JsonString(value: String) extends JsonValue {
//    override val tag = "jv-string";
//  }
//
//  @JSExportAll
//  case class JsonBoolean(value: Boolean) extends JsonValue {
//    override val tag = "jv-boolean";
//  }
//
//  @JSExportAll
//  case class JsonNumber(value: String) extends JsonValue {
//    override val tag = "jv-number";
//  }
//
//  @JSExportAll
//  case class JsonArray(elems: js.Array[JsonValue]) extends JsonValue {
//    override val tag = "jv-array";
//  }
//
//  @JSExportAll
//  case class JsonObject(properties: js.Array[JsonProperty]) extends JsonValue {
//    override val tag = "jv-object";
//  }
//
//  @JSExportAll
//  case class JsonProperty(name: String, value: JsonValue)

  def astToJsonValue(v: Ast.Value): js.Any = v match {
    case Ast.Null(_) =>
      js.Dynamic.literal("tag" -> "jv-null")
    case Ast.Object(_, attributes) =>
      js.Dynamic.literal(
        "tag" -> "jv-object",
        "properties" -> attributes.map(a => js.Dynamic.literal("name" -> a.name.s, "value" -> astToJsonValue(a.value))).toJSArray
      )
    case Ast.Array(_, elems) =>
      js.Dynamic.literal(
        "tag" -> "jv-array",
        "elems" -> elems.map(astToJsonValue).toJSArray
      )
    case Ast.Number(_, v) =>
      js.Dynamic.literal("tag" -> "jv-number", "value" -> v)
    case Ast.Str(_, v) =>
      js.Dynamic.literal("tag" -> "jv-string", "value" -> v)
    case Ast.Bool(_, v) =>
      js.Dynamic.literal("tag" -> "jv-boolean", "value" -> v)
  }

}
