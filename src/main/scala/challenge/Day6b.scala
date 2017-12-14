package challenge

import base.Challenge

import scala.annotation.tailrec

object Day6b extends Challenge {

  def findLoopLength(xs: Vector[Int]): Int = {

    @tailrec
    def distribute(xs: Vector[Int], i: Int, n: Int): Vector[Int] = n match {
      case 0 => xs
      case _ =>
        distribute(xs.updated(i % xs.length, xs(i % xs.length) + 1), i + 1, n - 1)
    }

    @tailrec
    def accumulator(xs: Vector[Int], history: List[List[Int]]): Int = {
      if (history.contains(xs.toList)) history.length - history.indexOf(xs.toList)
      else {
        val max = xs.max
        val src = xs.zipWithIndex.filter(_._1 == max).minBy(_._2)
        accumulator(distribute(xs.updated(src._2, 0), src._2 + 1, src._1), history :+ xs.toList)
      }
    }

    accumulator(xs, List())
  }

  override def run(): Any = {
    val input = Vector(0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11)
    findLoopLength(input)
  }

}



