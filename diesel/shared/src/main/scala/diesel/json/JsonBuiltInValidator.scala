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

package diesel.json

import diesel.Marker
import diesel.SimpleMarkerMessage
import diesel.Marker.{Descriptor, Kind, Severity}
import diesel.json.Ast.Value

trait JsonBuiltInValidator {

  def validate(value: Value): Seq[Marker]

}

object JsonBuiltInValidator {

  def validate(value: Value): Seq[Marker] =
    value match {
      case Ast.Object(_, attributes) =>
        attributes.groupBy(_.name.s).filter(_._2.length > 1).values.toSeq.flatten.map(attr =>
          Marker(
            Descriptor(Kind.Semantic, Severity.Error),
            attr.name.position.offset,
            attr.name.position.length,
            SimpleMarkerMessage(s"Duplicated attribute ${attr.name.s}")
          )
        ) ++ attributes.flatMap(a => validate(a.value))
      case Ast.Array(_, elems)       =>
        elems.flatMap(validate)
      case _                         =>
        Seq()
    }

//  def validateJsModel(value: Value, jsModel: JsModel): Seq[Marker] = new JsModelValidator(jsModel).validate(value)
//
//  def missingAttributes(jsModel: JsModel, className: String, obj: Ast.Object): Seq[JsAttr] = {
//    val existingAttrNames = obj.attributes.map(_.name.s).toSet
//    val declaredAttrs     = jsModel.findAttributes(className)
//    declaredAttrs.filter(jsAttr => jsAttr.required && !existingAttrNames.contains(jsAttr.name))
//  }
//
//  private class JsModelValidator(val jsModel: JsModel) extends JsonValidator {
//    override def validate(value: Value): Seq[Marker] = doValidate(value, JsPath.empty)
//
//    private def doValidate(value: Value, path: JsPath): Seq[Marker] = {
//      jsModel.getExpectedType(path)
//        .map(jsModel.getDiscriminatedType(value, _))
//        .map {
//          case JsAny                       =>
//            Seq()
//          case s @ JsString                =>
//            value match {
//              case Str(_, _) =>
//                Seq()
//              case x @ _     =>
//                Seq(unexpectedValueError(x.position, s))
//            }
//          case n @ JsNumber                =>
//            value match {
//              case Ast.Number(_, _) =>
//                Seq()
//              case x @ _            =>
//                Seq(unexpectedValueError(x.position, n))
//            }
//          case b @ JsBoolean               =>
//            value match {
//              case Ast.Bool(_, _) =>
//                Seq()
//              case x @ _          =>
//                Seq(unexpectedValueError(x.position, b))
//            }
//          case r @ JsDomainRef(domainName) =>
//            value match {
//              case Ast.Str(position, value) =>
//                jsModel.domains.find(_.name == domainName) match {
//                  case Some(JsDomain(_, values)) =>
//                    if (values().contains(value)) {
//                      Seq()
//                    } else {
//                      Seq(invalidDomainItemError(position, value))
//                    }
//                  case None                      =>
//                    Seq(unexpectedValueError(position, r))
//                }
//              case _                        =>
//                Seq.empty
//            }
//
//          case c @ JsClassRef(className) =>
//            value match {
//              case o @ Ast.Object(_, attributes) =>
//                val declaredAttrs         = jsModel.findAttributes(className)
//                val declaredAttrNames     = declaredAttrs.map(_.name).toSet
//                // check if we have undeclared attributes
//                val undeclaredAttrMarkers = attributes.filter(attr => !declaredAttrNames.contains(attr.name.s)).map(
//                  attr => undeclaredAttributeError(attr)
//                )
//                // check if we have missing attributes
//                val missingAttrs          = missingAttributes(jsModel, className, o)
//                val missingAttrsMarkers   =
//                  if (missingAttrs.isEmpty)
//                    Seq()
//                  else
//                    Seq(missingAttributesError(o, missingAttrs))
//
//                // and recurse
//                undeclaredAttrMarkers ++ missingAttrsMarkers ++ attributes.flatMap(attr =>
//                  doValidate(attr.value, path.appendField(attr.name.s))
//                )
//              case x @ _                         =>
//                Seq(unexpectedValueError(x.position, c))
//            }
//
//          case a @ JsArray(_) =>
//            value match {
//              case Ast.Array(_, elems) =>
//                elems.zipWithIndex.flatMap {
//                  case (v, i) =>
//                    doValidate(v, path.appendArrayAccess(i))
//                }
//              case x @ _               =>
//                Seq(unexpectedValueError(x.position, a))
//            }
//        }.getOrElse(Seq())
//    }
//
//    private def invalidDomainItemError(pos: Ast.Position, value: String): Marker =
//      Marker(
//        Descriptor(Kind.Semantic, Severity.Error),
//        pos.offset,
//        pos.length,
//        s"Invalid domain value $value"
//      )
//
//    private def unexpectedValueError(pos: Position, expectedType: JsType): Marker =
//      Marker(
//        Descriptor(Kind.Semantic, Severity.Error),
//        pos.offset,
//        pos.length,
//        s"Unexpected value : expecting ${expectedType.name}"
//      )
//
//    private def undeclaredAttributeError(attr: Ast.Attribute): Marker =
//      Marker(
//        Descriptor(Kind.Semantic, Severity.Error),
//        attr.name.position.offset,
//        attr.name.position.length,
//        s"Undeclared attribute : ${attr.name.s}"
//      )
//
//    private def missingAttributesError(obj: Ast.Object, missing: Seq[JsAttr]): Marker =
//      Marker(
//        Descriptor(Kind.Semantic, Severity.Error),
//        obj.position.offset,
//        1,
//        s"Missing attribute(s) : ${missing.map(_.name).sorted.mkString(", ")}"
//      )
//
//  }

}
