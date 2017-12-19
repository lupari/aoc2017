package challenge

import base.Challenge
import challenge.Day19.Square

import scala.io.Source

object Day19b extends Challenge {

  override def run(): Any = {
    val input: List[List[Char]] = Source.fromResource("day19.txt").getLines().toList.map(_.toList)
    val maxLineWidth = input.map(_.length).max
    val wideInput = input.map(l => l ++ List.fill(maxLineWidth - l.length)(' '))
    val grid = wideInput.zipWithIndex.map(y => y._1.zipWithIndex.map(x => Square(x._2, y._2, x._1)))
    val path: List[Square] = Day19.travel(grid)
    path.length
  }

}
