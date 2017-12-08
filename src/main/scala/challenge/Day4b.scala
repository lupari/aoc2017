package challenge

import base.Challenge

import scala.io.Source

object Day4b extends Challenge {

  def isValid(xs: List[String]): Boolean = {
    val sorted = xs.map(w => w.sorted)
    sorted.distinct.size == sorted.size
  }

  override def run(): Unit = {
    val input: List[List[String]] = Source.fromResource("day4.txt").getLines.toList
      .map(s => s.split(" ").toList)

    println(input.count(i => isValid(i)))
  }

}



