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

import munit.FunSuite
import scala.util.Try
import scala.util.Failure
import scala.util.Success

class MarkResetSourceTest extends FunSuite {

  test("no mark") {
    val text = "0123456789"
    val mis  = MarkResetSource.fromString(text)
    assertEquals(mis.next(), '0')
    assertEquals(mis.next(), '1')
  }

  test("mark reset") {
    val text = "0123456789"
    val mis  = MarkResetSource.fromString(text)
    assertEquals(mis.next(), '0')
    mis.mark()
    mis.reset()
    assertEquals(mis.next(), '1')
    assertEquals(mis.next(), '2')
    assertEquals(mis.next(), '3')
  }

  test("next and then mark") {
    val text = "0123456789"
    val mis  = MarkResetSource.fromString(text)
    assertEquals(mis.next(), '0')
    assertEquals(mis.next(), '1')
    mis.mark()
    assertEquals(mis.next(), '2')
    assertEquals(mis.next(), '3')
    mis.reset()
    assertEquals(mis.next(), '2')
    assertEquals(mis.next(), '3')
  }

  test("mark reset multiple") {
    val text = "0123456789"
    val mis  = MarkResetSource.fromString(text)
    mis.mark()
    assertEquals(mis.next(), '0')
    assertEquals(mis.next(), '1')
    mis.reset()
    assertEquals(mis.next(), '0')
    assertEquals(mis.next(), '1')
    assertEquals(mis.next(), '2')
    mis.mark()
    assertEquals(mis.next(), '3')
    assertEquals(mis.next(), '4')
    mis.reset()
    assertEquals(mis.next(), '3')
    assertEquals(mis.next(), '4')
    assertEquals(mis.next(), '5')
  }

  test("reset not marked") {
    val text = "0123456789"
    val mis  = MarkResetSource.fromString(text)
    assertEquals(mis.next(), '0')
    assertEquals(mis.next(), '1')
    Try(mis.reset()) match {
      case Failure(exception) =>
        // all good !
        assertEquals(exception.getMessage(), "not marked")
      case Success(_)         =>
        fail("should have thrown")
    }
  }

  test("has next no marks") {
    val text = "01"
    val mis  = MarkResetSource.fromString(text)
    assert(mis.hasNext)
    assert(mis.hasNext)
    assertEquals(mis.next(), '0')
    assert(mis.hasNext)
    assertEquals(mis.next(), '1')
    assert(!mis.hasNext)
    assert(!mis.hasNext)
  }

  test("like lexer") {
    val text = "0123456789"
    val mis  = MarkResetSource.fromString(text)
    assert(mis.hasNext)
    mis.mark()
    assert(mis.hasNext)
    assertEquals(mis.next(), '0')
    assert(mis.hasNext)
    assertEquals(mis.next(), '1')
    assert(mis.hasNext)
    mis.reset()
    assert(mis.hasNext)
    mis.mark()
    assert(mis.hasNext)
    assertEquals(mis.next(), '0')
    assert(mis.hasNext)
    assertEquals(mis.next(), '1')
    assert(mis.hasNext)
    mis.reset()
    assert(mis.hasNext)
    var i    = 0
    while (i < text.length()) {
      assertEquals(mis.next(), i.toString().charAt(0))
      if (i < text.length() - 1) {
        assert(mis.hasNext)
      }
      i += 1
    }
    assert(!mis.hasNext)
  }

}
