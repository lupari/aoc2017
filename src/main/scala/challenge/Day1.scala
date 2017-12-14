package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends Challenge {

  def getCaptcha(xs: List[Char]): Int = {

    val first = xs.head

    @tailrec
    def accumulator(xs: List[Char], acc: Int): Int = xs match {
      case h :: i :: t =>
        if (h == i) accumulator(i :: t, acc + i.asDigit)
        else accumulator(i :: t, acc)
      case h :: Nil => accumulator(Nil, if (h == first) acc + h.asDigit else acc)
      case _ => acc
    }

    accumulator(xs, 0)
  }

  override def run(): Any = {
    val input = Source.fromResource("day1.txt").getLines.mkString.toList
    getCaptcha(input)
  }

}



