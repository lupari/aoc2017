package challenge

import base.Challenge

import scala.io.Source

object Day9b extends Challenge {

  def readStream(xs: List[Char]): Int = {

    def accumulator(xs: List[Char], acc: Int, garbage: Boolean): Int = xs match {
      case h :: t =>
        if (garbage) h match {
          case '!' => accumulator(t drop 1, acc, garbage)
          case '>' => accumulator(t, acc, garbage = false)
          case _ => accumulator(t, acc + 1, garbage)
        } else h match {
          case '<' => accumulator(t, acc, garbage = true)
          case _ => accumulator(t, acc, garbage)
        }
      case _ => acc
    }

    accumulator(xs, 0, garbage = false)
  }

  override def run(): Unit = {
    val input: List[Char] = Source.fromResource("day9.txt").getLines().mkString.toList
    println(readStream(input))
  }

}



