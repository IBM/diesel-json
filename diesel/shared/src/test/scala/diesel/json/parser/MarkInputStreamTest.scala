package diesel.json.parser

import munit.FunSuite

class MarkInputStreamTest extends FunSuite {

  test("no mark") {
    val text = "0123456789"
    val mis  = MarkInputStream.fromString(text)
    val s    = mis.toSeq.mkString
    assertEquals(s, text)
  }

  test("mark once beginning") {
    val text = "0123456789"
    val mis  = MarkInputStream.fromString(text)
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

}
