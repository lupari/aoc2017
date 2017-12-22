package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day22 extends Challenge {

  case class Position(x: Int, y: Int)  {
    def +(delta: Delta) = Position(x + delta.x, y + delta.y)
  }
  case class Delta(x: Int, y: Int)

  object Delta {
    def up: Delta = Delta(0, -1)

    def down: Delta = Delta(0, 1)

    def left: Delta = Delta(-1, 0)

    def right: Delta = Delta(1, 0)
  }

  def spread(input: List[List[Char]], limit: Int): Int = {

    def nextDir(dir: Delta, isClean: Boolean): Delta = dir match {
      case Delta(0, -1) => if (isClean) Delta.left else Delta.right
      case Delta(0, 1) => if (isClean) Delta.right else Delta.left
      case Delta(-1, 0) => if (isClean) Delta.down else Delta.up
      case Delta(1, 0) => if (isClean) Delta.up else Delta.down
      case _ => throw new NoSuchElementException
    }

    @tailrec
    def accumulator(pos: Position, dir: Delta, i: Int, contaminated: Int, acc: Map[Position, Boolean]): Int = i match {
      case n if n == limit =>
        contaminated
      case _ =>
        val isClean = acc(pos)
        val newDir = nextDir(dir, isClean)
        val inc = if (isClean) 1 else 0
        accumulator(pos + newDir, newDir, i + 1, contaminated + inc, acc + (pos -> !isClean))
    }

    val grid: Map[Position, Boolean] = input.zipWithIndex.flatMap(
      y => y._1.zipWithIndex.map(
        x => (Position(x._2, y._2), x._1 == '.'))
    ).toMap
    val startPos = Position(grid.keys.map(_.x).max / 2, grid.keys.map(_.y).max / 2)

    accumulator(startPos, Delta.up, 0, 0, grid.withDefaultValue(true))
  }

  override def run(): Any = {
    val input: List[List[Char]] = Source.fromResource("day22.txt").getLines.toList.map(_.toList)
    spread(input, 10000)
  }

}
