package challenge

import base.Challenge

object Day25 extends Challenge {

  def play(limit: Int): Int = {

    def accumulator(pos: Int, state: Char, i: Int, tape: Map[Int, Boolean]): Int = i match {
      case x if x == limit => tape.values.count(_ == true)
      case _ => state match {
        case 'a' =>
          val v = tape(pos)
          accumulator(pos + (if (!v) 1 else -1), 'b', i + 1, tape + (pos -> !v))
        case 'b' =>
          val v = tape(pos)
          accumulator(pos + (if (!v) 1 else -1), if (!v) 'c' else 'b', i + 1, tape)
        case 'c' =>
          val v = tape(pos)
          accumulator(pos + (if (!v) 1 else -1), if (!v) 'd' else 'a', i + 1, tape + (pos -> !v))
        case 'd' =>
          val v = tape(pos)
          accumulator(pos - 1, if (!v) 'e' else 'f', i + 1, tape + (pos -> true))
        case 'e' =>
          val v = tape(pos)
          accumulator(pos -1, if (!v) 'a' else 'd', i + 1, tape + (pos -> !v))
        case 'f' =>
          val v = tape(pos)
          accumulator(pos + (if (!v) 1 else -1), if (!v) 'a' else 'e', i + 1, tape + (pos -> true))
        case _ => throw new NoSuchElementException
      }

    }

    accumulator(0, 'a', 0, Map().withDefaultValue(false))

  }

  override def run(): Any = {
    play(12586542)
  }

}
