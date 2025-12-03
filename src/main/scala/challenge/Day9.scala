package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day9 extends Challenge {

  def readStream(xs: List[Char]): Int = {

    @tailrec
    def accumulator(xs: List[Char], opened: Int, acc: Int, garbage: Boolean): Int = xs match {
      case h :: t =>
        if (garbage && h != '>' && h != '!') accumulator(t, opened, acc, garbage)
        else h match {
          case '{' => accumulator(t, opened + 1, acc, garbage)
          case '}' => accumulator(t, if (opened > 0) opened - 1 else opened, if (opened > 0) acc + opened else acc, garbage)
          case '!' => accumulator(t drop 1, opened, acc, garbage)
          case '<' => accumulator(t, opened, acc, garbage = true)
          case '>' => accumulator(t, opened, acc, garbage = false)
          case _ => accumulator(t, opened, acc, garbage)
        }
      case _ => acc
    }

    accumulator(xs, 0, 0, garbage = false)
  }

  override def run(): Any = {
    val input: List[Char] = Source.fromResource("day9.txt").getLines().mkString.toList
    readStream(input)
  }

}



