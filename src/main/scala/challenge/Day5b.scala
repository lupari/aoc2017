package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day5b extends Challenge {

  def jumpCount(xs: Vector[Int], inc: Int => Int): Int = {

    @tailrec
    def accumulator(xs: Vector[Int], pos: Int, acc: Int): Int = {
      xs.lift(pos) match {
        case Some(offset) => accumulator(xs.updated(pos, offset + inc(offset)), pos + offset, acc + 1)
        case _ => acc
      }
    }

    accumulator(xs, 0, 0)
  }

  override def run(): Any = {
    val input: Vector[Int] = Source.fromResource("day5.txt").getLines.toList.map(_.toInt).toVector
    jumpCount(input, x => if (x > 2) -1 else 1)
  }

}



