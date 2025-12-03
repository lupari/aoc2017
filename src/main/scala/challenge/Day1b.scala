package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day1b extends Challenge {

  def getCaptcha(xs: List[Char]): Int = {
    val origin = xs

    def lookup(pos: Int): Char = List.concat(origin, origin)(pos + origin.length / 2)

    @tailrec
    def accumulator(xs: List[Char], pos: Int, acc: Int): Int = xs match {
      case h :: t =>
        accumulator(t, pos + 1, if (h == lookup(pos)) acc + h.asDigit else acc)
      case _ => acc
    }

    accumulator(xs, 0, 0)
  }

  override def run(): Any = {
    val input = Source.fromResource("day1.txt").getLines.mkString.toList
    getCaptcha(input)
  }
}
