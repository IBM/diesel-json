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

package diesel.json.i18n

import diesel.i18n.Messages
import diesel.i18n.Loader
import diesel.i18n.Lang
import diesel.i18n.MessageFormat

object I18n extends Messages {
  import Messages.{KeyResolver, Lang, Resolver}

  val invalidType: Msg1[Seq[String]]                       = msg1[Seq[String]].arg(pipedToString)
  val invalidStringLengthMin: Msg1[Int]                    = msg1[Int]
  val invalidStringLengthMax: Msg1[Int]                    = msg1[Int]
  val notMatchingStringPattern: Msg1[String]               = msg1[String]
  val missingRequiredProperties: Plural[Msg1[Seq[String]]] = plural(msg1[Seq[String]](_).arg(commaSeparatedToString))
  val invalidPropertyName: Msg2[String, Seq[String]]       = msg2[String, Seq[String]].arg2(commaSeparatedToString)
  val invalidArrayLength: Msg1[Int]                        = msg1[Int]
  val nothingValidates                                     = msg0
  val valueNotInEnum: Msg1[Seq[String]]                    = msg1[Seq[String]].arg(pipedToString)
  val invalidConstant: Msg1[String]                        = msg1[String]
  val notFailed                                            = msg0
  val invalidStringFormat: Msg1[String]                    = msg1[String]

  private def pipedToString[T] = { arg: Seq[T] => arg.mkString(" | ") }

  override protected def load(): Map[Messages.Lang, Map[String, MessageFormat]] =
    I18nFiles.messages.flatMap { case (k, v) =>
      Lang(k).flatMap(lang => Loader.loadProperties(v).map((lang, _)))
    }.toMap

  override protected def newResolver(loaded: Map[Messages.Lang, Map[String, MessageFormat]]): Messages.Resolver =
    Resolver(loaded).withFallback(Lang.EN)

  implicit var currentKeyResolver: KeyResolver = keyResolver(Lang.EN)

  def setLang(lang: Lang): Unit = {
    currentKeyResolver = keyResolver(lang)
  }
}
