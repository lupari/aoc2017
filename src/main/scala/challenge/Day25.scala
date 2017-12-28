package challenge

import base.Challenge

import scala.annotation.tailrec

object Day25 extends Challenge {

  def play(limit: Int): Int = {

    @tailrec
    def accumulator(i: Int, pos: Int, state: Char, tape: Map[Int, Boolean]): Int = i match {
      case x if x == limit => tape.values.count(_ == true)
      case _ =>
        val v = tape(pos)
        state match {
          case 'a' =>
            if (v) accumulator(i + 1, pos - 1, 'b', tape + (pos -> false))
            else accumulator(i + 1, pos + 1, 'b', tape + (pos -> true))
          case 'b' =>
            if (v) accumulator(i + 1, pos - 1, 'b', tape)
            else accumulator(i + 1, pos + 1, 'c', tape)
          case 'c' =>
            if (v) accumulator(i + 1, pos - 1, 'a', tape + (pos -> false))
            else accumulator(i + 1, pos + 1, 'd', tape + (pos -> true))
          case 'd' =>
            if (v) accumulator(i + 1, pos - 1, 'f', tape + (pos -> true))
            else accumulator(i + 1, pos - 1, 'e', tape + (pos -> true))
          case 'e' =>
            if (v) accumulator(i + 1, pos - 1, 'd', tape + (pos -> false))
            else accumulator(i + 1, pos - 1, 'a', tape + (pos -> true))
          case 'f' =>
            if (v) accumulator(i + 1, pos - 1, 'e', tape + (pos -> true))
            else accumulator(i + 1, pos + 1, 'a', tape + (pos -> true))
          case _ => throw new NoSuchElementException
        }
    }

    accumulator(0, 0, 'a', Map().withDefaultValue(false))

  }

  override def run(): Any = {
    play(12586542)
  }

}
