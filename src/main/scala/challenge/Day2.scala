package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends Challenge {

  def getChecksum(xs: List[List[Int]]): Long = {

    @tailrec
    def accumulator(xs: List[List[Int]], acc: Long): Long = xs match {
      case h :: t => accumulator(t, acc + h.max - h.min)
      case _ => acc
    }

    accumulator(xs, 0)
  }

  override def run(): Unit = {
    val input: List[List[Int]] = Source.fromResource("day2.txt").getLines.toList
      .map(s => s.split("\t")
        .map(s => s.toInt)
        .toList)
    println(getChecksum(input))
  }

}



