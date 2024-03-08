package diesel.json.parser

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object MarkInputStream {

  def fromSource(source: Source): MarkInputStream = new MarkInputStream(source)

  def fromString(s: String): MarkInputStream = fromSource(Source.fromString(s))

}

class MarkInputStream(private val source: Source) {

  private var index                  = 0
  private var markPos                = -1
  private var resetPos               = -1
  private var buf: ArrayBuffer[Char] = ArrayBuffer()

  def hasNext: Boolean = source.hasNext

  def next(): Char = {
    if (markPos == -1) {
      // not marked, just move forward
      index = index + 1
      source.next()
    } else {
      // marked, check if we're inside buf
      val bufEnd = markPos + buf.size
      if (index < bufEnd) {
        val c = buf.apply(index - markPos)
        index = index + 1
        c
      } else {
        // outside buf -> append
        val c = source.next()
        index = index + 1
        buf.append(c)
        c
      }
    }
  }

  def mark(): Unit = {
    markPos = index
  }

  def reset(): Unit = {
    resetPos = index
  }

  def toSeq: Seq[Char] = source.toSeq

}
