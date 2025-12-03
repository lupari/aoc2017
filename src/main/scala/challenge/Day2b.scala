package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day2b extends Challenge {

  def getChecksum(xs: List[List[Int]]): Long = {

    def findDivision(xs: List[Int]): Int = {
      val pair = xs.sorted.flatMap(x => xs.map(y => (x, y)))
        .filter(pair => pair._2 != pair._1)
        .find(pair => pair._2 % pair._1 == 0).get
      pair._2 / pair._1
    }

    @tailrec
    def accumulator(xs: List[List[Int]], acc: Long): Long = xs match {
      case h :: t => accumulator(t, acc + findDivision(h))
      case _ => acc
    }

    accumulator(xs, 0)
  }

  override def run(): Any = {
    val input: List[List[Int]] = Source.fromResource("day2.txt").getLines.toList.map(_.split("\t").map(_.toInt).toList)
    getChecksum(input)
  }

}



