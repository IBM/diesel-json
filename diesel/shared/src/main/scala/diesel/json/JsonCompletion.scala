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

import diesel.Bnf.DslSyntax
import diesel._
import diesel.json.jsonschema.{JPath, JsonSchema}

object JsonCompletion {

  def completionConfiguration(schema: JsonSchema): CompletionConfiguration = {
    val configuration = new CompletionConfiguration
    val valueProvider = new JsonValueProvider(schema)
    configuration.setProvider(DslSyntax(Json.attrName), new AttrNameProvider(schema))
    configuration.setProvider(DslSyntax(Json.arrayValue), valueProvider)
    configuration.setProvider(DslSyntax(Json.attrValue), valueProvider)
    configuration.setProvider(DslSyntax(Json.sAxiom), valueProvider)
    configuration.setFilter(new JsonCompletionFilter(schema))
    configuration
  }

  private def reduceProposals(values: Seq[Ast.Value]): Seq[Ast.Value] = values

  def getSchemaProposals(schema: JsonSchema, rootValue: Ast.Value, path: JPath): Seq[Ast.Value] = {
    val validationResult = schema.validate(rootValue)
    val resultsAtPath    = validationResult.flatten.filter(_.path == path)
    val ps               = resultsAtPath.flatMap(_.getProposals()).distinct
    reduceProposals(ps)
  }

  sealed trait ProposalKind
  case object AttrNameProposal           extends ProposalKind
  case class ValueProposal(v: Ast.Value) extends ProposalKind

  private def customProposal(text: String, kind: ProposalKind) =
    CompletionProposal(None, text, None, Some(kind))

  private def createDieselValueProposals(
    schemaProposals: Seq[Ast.Value]
  ): Seq[CompletionProposal] = {
    val labels: Seq[(Ast.Value, String)] = schemaProposals
      .flatMap {
        case x: Ast.Null   =>
          Seq((x, "null"))
        case x: Ast.Object =>
          Seq((x, "{"))
        case x: Ast.Array  =>
          Seq((x, "["))
        case x: Ast.Number =>
          Seq((x, x.v.toInt.toString)) // TODO we convert to int just to have "0" in preds and not "0.0"...
        case x: Ast.Str    =>
          Seq((x, "\"" + x.v + "\""))
        case x: Ast.Bool   =>
          Seq((x, x.v.toString))
      }
    labels.map(l => customProposal(l._2, ValueProposal(l._1)))
  }

  class AttrNameProvider(private val schema: JsonSchema) extends CompletionProvider {
    override def getProposals(
      element: Option[Bnf.DslElement],
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode]
    ): Seq[CompletionProposal] =
      node.map { n =>
        val path0            = JPath.pathToNode(n, offset)
        val path             = if (n.offset > offset) path0.parent.getOrElse(path0) else path0
        val rootValue        = tree.root.value.asInstanceOf[Ast.Value]
        val validationResult = schema.validate(rootValue)
        val resultsAtPath    = validationResult.flatten.filter(_.path == path)
        val ps               = resultsAtPath.flatMap(_.getProposals()).distinct
        val reduced          = reduceProposals(ps)
        path.resolve(rootValue)
          .map { valueAtPath =>
            val existingAttrs = valueAtPath
              .asAstObject
              .map(_.attributes)
              .map(_.map(_.name.s))
              .getOrElse(Seq.empty)
              .toSet
            val props         = reduced.flatMap {
              case Ast.Object(_, attributes) =>
                attributes
                  .filter { a =>
                    !existingAttrs.contains(a.name.s)
                  }
                  .sortBy(_.name.s)
                  .map { a =>
                    customProposal("\"" + a.name.s + "\"", AttrNameProposal)
                  }
              case _                         =>
                Seq.empty
            }
            props
          }
          .getOrElse(Seq.empty)
      }.getOrElse(Seq.empty)
  }

  class JsonValueProvider(private val schema: JsonSchema) extends CompletionProvider {

    override def getProposals(
      element: Option[Bnf.DslElement],
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode]
    ): Seq[CompletionProposal] =
      node.map { n =>
        val path            = JPath.pathToNode(n, offset)
        val rootValue       = tree.root.value.asInstanceOf[Ast.Value]
        val schemaProposals = getSchemaProposals(schema, rootValue, path)
        createDieselValueProposals(schemaProposals)
      }.getOrElse(Seq.empty)
  }

  class JsonCompletionFilter(private val schema: JsonSchema) extends CompletionFilter {

    override def filterProposals(
      tree: GenericTree,
      offset: Int,
      node: Option[GenericNode],
      proposals: Seq[CompletionProposal]
    ): Seq[CompletionProposal] = {
      if (proposals.exists(_.text == ",")) {
        proposals
      } else {

        node.map { n =>
          val rootVal         = tree.root.value.asInstanceOf[Ast.Value]
          val customProposals = proposals.filter(_.userData.isDefined)

          val attrProposals = customProposals.exists(_.userData.exists(_ == AttrNameProposal))
          if (attrProposals) {
            Seq(CompletionProposal(None, "}")) ++
              customProposals ++
              Seq(CompletionProposal(None, "\"\""))
          } else {
            val valueProposals = customProposals.exists(_.userData.exists(_.isInstanceOf[ValueProposal]))

            if (valueProposals) {

              // add closing array / object depending on schema proposals
              val arrayOpen = customProposals
                .find(_.userData.exists {
                  case ValueProposal(v) =>
                    v.asAstArray.isDefined
                  case _                =>
                    false
                })
                .map { _ =>
                  Seq(CompletionProposal(None, "["))
                }
                .getOrElse(Seq.empty)

              val objectOpen = customProposals
                .find(_.userData.exists {
                  case ValueProposal(v) =>
                    v.asAstObject.isDefined
                  case _                =>
                    false
                })
                .map { _ =>
                  Seq(CompletionProposal(None, "{"))
                }
                .getOrElse(Seq.empty)

              // check if parent is an object or an array
              // and propose closing ] or }
              val closingSymbol = JPath.pathToNode(n, offset)
                .parent
                .flatMap(_.resolve(rootVal))
                .map {
                  case _: Ast.Object =>
                    if (proposals.exists(_.text == "}")) {
                      Seq("}")
                    } else {
                      Seq.empty
                    }
                  case _: Ast.Array  =>
                    Seq("]")
                  case _             =>
                    Seq.empty
                }
                .getOrElse(Seq.empty)
                .map(CompletionProposal(None, _))

              (customProposals ++
                arrayOpen ++
                objectOpen ++
                closingSymbol).distinctBy(_.text)

            } else {
              proposals
            }
          }
        }.getOrElse(proposals)

      }
    }
  }
}
