package challenge

import base.Challenge

object Day14 extends Challenge {

  override def run(): Any = {
    val input: List[List[Int]] = (0 to 127).map("ljoxqyyw-" + _).map(_.toList.map(_.toInt)).toList
    val lines: List[String] = input.map(i => BigInt(Day10b.hash(i), 16).toString(2))
    lines.foldLeft(0)((a, b) => a + b.toList.count(_ == '1'))
  }

}
