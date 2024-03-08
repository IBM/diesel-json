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
import scala.collection.mutable.ArrayBuffer

object MarkResetSource {

  def fromSource(source: Source): MarkResetSource = new MarkResetSource(source)

  def fromString(s: String): MarkResetSource = fromSource(Source.fromString(s))

}

class MarkResetSource(private val source: Source) {

  private var index                  = 0
  private var buf: ArrayBuffer[Char] = ArrayBuffer()
  // private var marked                 = false
  private var markPos                = -1

  def hasNext: Boolean = {
    if (markPos == -1) {
      source.hasNext
    } else {
      val bufIndex = index - markPos
      if (bufIndex < buf.length) {
        true
      } else {
        source.hasNext
      }
    }
  }

  def next(): Char = {
    if (markPos == -1) {
      index = index + 1
      source.next()
    } else {
      val bufIndex = index - markPos
      if (bufIndex < buf.length) {
        val c = buf.apply(bufIndex)
        index = index + 1
        c
      } else {
        val c = source.next()
        buf.append(c)
        index = index + 1
        c
      }
    }
    // }
  }

  def mark(): Unit = {
    val bufIndex = index - markPos
    if (bufIndex >= buf.length) {
      buf = ArrayBuffer()
    }
    markPos = index
  }

  def reset(): Unit = {
    if (markPos != -1) {
      index = markPos
    } else {
      throw new RuntimeException("not marked")
    }
  }

}
