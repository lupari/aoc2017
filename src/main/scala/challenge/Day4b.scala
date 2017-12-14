package challenge

import base.Challenge

import scala.io.Source

object Day4b extends Challenge {

  def isValid(xs: List[String]): Boolean = {
    val sorted = xs.map(_.sorted)
    sorted.distinct.size == sorted.size
  }

  override def run(): Any = {
    val input: List[List[String]] = Source.fromResource("day4.txt").getLines.toList.map(_.split(" ").toList)
    input.count(isValid)
  }

}



