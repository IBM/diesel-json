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

package diesel.json.jsonschema

import diesel.Bnf.DslSyntax
import diesel.json.Ast
import diesel.{GenericNode, GenericNonTerminal}

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

case object JPath {
  val empty: JPath = JPath(Seq())

  object Implicits {

    implicit def strToPath(s: String): JPath = parsePath(s)

  }

  // naive, TODO handle escaping of /
  def parsePath(path: String): JPath = {
    val s = path
      .dropWhile(_ == '/')
      .reverse
      .dropWhile(_ == '/')
      .reverse
    if (s.isEmpty) {
      JPath.empty
    } else {
      JPath(s.split('/').toSeq)
    }
  }

//  def pathToOffset(tree: GenericTree, offset: Int): Option[JPath] = {
//    val node = getNodeAt(tree, offset)
//    node.map(pathToNode(_, offset))
//  }
//
//  def getNodeAt(tree: GenericTree, offset: Int): Option[GenericNode] = {
//    val nodes    = tree.root.findNodesAtOffset(offset)
//    val filtered = nodes.filter(_.isInstanceOf[GenericNonTerminal])
//    val sorted   = filtered.toSeq.sortBy(_.getParents.size).reverse
//    sorted.headOption
//  }

  def pathToNode(node: GenericNode, offset: Int): JPath = {
    node match {
      case terminal: GenericNonTerminal =>
        terminal.production.element match {
          case Some(DslSyntax(syntax)) =>
            syntax.name match {
              case "attr"       =>
                // only handle first child
                val thisPath =
                  if (
                    node.parent.exists(_.getElement.exists({
                      case DslSyntax(syntax) => syntax.name == "attr"
                      case _                 => false
                    }))
                  ) {
                    // skip
                    JPath.empty
                  } else {
                    node.value match {
                      case attr: Ast.Attribute =>
                        val attrName = attr.name.s
                        if (attrName.isEmpty) {
                          JPath.empty
                        } else {
                          JPath.empty.append(attrName)
                        }
                      case _                   =>
                        JPath.empty
                    }
                  }

                node.parent
                  .map(pathToNode(_, offset))
                  .getOrElse(JPath.empty)
                  .append(thisPath)
              case "arrayValue" =>
                // we need to find the index !
                val arrayNode = node.findFirstParent(_.value.isInstanceOf[Ast.Array])
                val array     = arrayNode.map(_.value.asInstanceOf[Ast.Array])
                val value     = node.value.asInstanceOf[Ast.Value]
                val elemIndex = array.map(a => a.elems.indexOf(value))
                val thisPath  =
                  elemIndex
                    .map(i => JPath.empty.append(i))
                    .getOrElse(JPath.empty)

                arrayNode
                  .map(pathToNode(_, offset))
                  .getOrElse(JPath.empty)
                  .append(thisPath)
              case "array_v"    =>
                // only append [0] if the array is empty and offset is in array !
                val thisPath =
                  if (
                    node.parent.exists(p =>
                      p.getElement.exists({
                        case DslSyntax(syntax) if syntax.name == "array_v" =>
                          true
                        case _                                             =>
                          false
                      })
                    )
                  ) {
                    // still on an array_v, move up
                    JPath.empty
                  } else {
                    // check if the array is empty
                    val arrayValue = node.value.asInstanceOf[Ast.Array]
                    if (arrayValue.elems.isEmpty && offset > arrayValue.position.offset) {
                      JPath.empty.append(0)
                    } else {
                      JPath.empty
                    }
                  }
                node.parent
                  .map(pathToNode(_, offset))
                  .getOrElse(JPath.empty)
                  .append(thisPath)

              case _ =>
                node.parent
                  .map(pathToNode(_, offset))
                  .getOrElse(JPath.empty)
            }
          case _                       =>
            node.parent
              .map(pathToNode(_, offset))
              .getOrElse(JPath.empty)
        }
      case _                            =>
        node.parent
          .map(pathToNode(_, offset))
          .getOrElse(JPath.empty)
    }
  }

}

case class JPath(elems: Seq[String]) {

  def append(elem: String): JPath = JPath(elems :+ elem)
  def append(path: JPath): JPath  = JPath(elems ++ path.elems)
  def append(index: Int): JPath   = append(index.toString)

  def parent: Option[JPath] = {
    if (elems.isEmpty) {
      None
    } else {
      Some(JPath(elems.dropRight(1)))
    }
  }

  def format: String = elems.mkString("/")

  def resolve(root: Ast.Value): Option[Ast.Value] = elems.toList match {
    case e :: rest =>
      val restPath = JPath(rest)
      // does it look  like an array ?
      Try(e.toInt) match {
        case Failure(_) =>
          // not a number, try the fields
          resolveField(restPath, e, root)

        case Success(index) =>
          // it's a number, look in array and then
          // in fields
          resolveArray(restPath, index, root) match {
            case Some(arrayValue) =>
              Some(arrayValue)
            case None             =>
              resolveField(restPath, e, root)
          }
      }
    case _         =>
      Some(root)
  }

  private def resolveArray(pathToValue: JPath, index: Int, value: Ast.Value): Option[Ast.Value] = {
    val arr  = value.asAstArray
    val elem = arr.flatMap(_.elemAt(index))
    elem.flatMap(pathToValue.resolve)
  }

  private def resolveField(pathToValue: JPath, name: String, value: Ast.Value): Option[Ast.Value] = {
    val obj  = value.asAstObject
    val attr = obj.flatMap(_.attr(name))
    attr.flatMap(pathToValue.resolve)
  }

  override def toString: String = "/" + elems.mkString("/")

  def isEmpty: Boolean = elems.isEmpty

  def isParentOf(child: JPath): Boolean =
    child.format.startsWith(this.format)

}
