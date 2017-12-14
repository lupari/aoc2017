package challenge

import base.Challenge

import scala.io.Source

object Day4 extends Challenge {

  def isValid(xs: List[String]): Boolean = {
    xs.distinct.size == xs.size
  }

  override def run(): Any = {
    val input: List[List[String]] = Source.fromResource("day4.txt").getLines.toList.map(_.split(" ").toList)
    input.count(isValid)
  }

}



