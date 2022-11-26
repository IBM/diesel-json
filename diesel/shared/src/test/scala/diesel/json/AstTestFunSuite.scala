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

import diesel.AstHelpers.{assertAst, assertNoMarkers}
import diesel.Dsl
import munit.FunSuite

abstract class AstTestFunSuite extends FunSuite {

  def testAst[T](text: String)(expected: => T)(implicit dsl: Dsl, loc: munit.Location): Unit = {
    val testName = getTestNameFromText(text)
    test(testName) {
      assertAst(dsl)(text) { tree =>
        assertNoMarkers(tree, true)
        assertEquals(tree.value.asInstanceOf[T], expected)
      }
    }
  }

  private def getTestNameFromText(text: String): String = {
    val trimmed         = text.trim
    val lines           = trimmed.split('\n')
    lazy val firstLine = {
      lines.filterNot(_.isEmpty)(0)
    }
    val oneLineTestName =
      if (lines.length > 1) { firstLine + s"...(${trimmed.length} on ${lines.length} lines)" }
      else { trimmed }
    oneLineTestName
  }

}
