package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day22b extends Challenge {

  case class Position(x: Int, y: Int) {
    def +(delta: Delta): Position = Position(x + delta.x, y + delta.y)
  }

  case class Delta(x: Int, y: Int) {
    def opposite: Delta = Delta(x * -1, y * -1)
  }

  def nextState(c: Char): Char = c match {
    case '.' => 'W'
    case 'W' => '#'
    case '#' => 'F'
    case 'F' => '.'
  }

  def spread(input: List[List[Char]], limit: Int): Int = {

    def nextDir(dir: Delta, state: Char): Delta = state match {
      case 'W' => dir
      case 'F' => dir.opposite
      case '.' => dir match {
        case Delta(0, -1) => Delta(-1, 0)
        case Delta(0, 1) => Delta(1, 0)
        case Delta(-1, 0) => Delta(0, 1)
        case Delta(1, 0) => Delta(0, -1)
        case _ => throw new NoSuchElementException
      }
      case '#' => dir match {
        case Delta(0, -1) => Delta(1, 0)
        case Delta(0, 1) => Delta(-1, 0)
        case Delta(-1, 0) => Delta(0, -1)
        case Delta(1, 0) => Delta(0, 1)
        case _ => throw new NoSuchElementException
      }
      case _ => throw new NoSuchElementException
    }

    @tailrec
    def accumulator(pos: Position, dir: Delta, i: Int, contaminated: Int, acc: Map[Position, Char]): Int = i match {
      case n if n == limit =>
        contaminated
      case _ =>
        val state: Char = acc(pos)
        val newDir = nextDir(dir, state)
        val inc = if (state == 'W') 1 else 0
        accumulator(pos + newDir, newDir, i + 1, contaminated + inc, acc + (pos -> nextState(state)))
    }

    val grid: Map[Position, Char] = input.zipWithIndex.flatMap(
      y => y._1.zipWithIndex.map(
        x => (Position(x._2, y._2), x._1))
    ).toMap
    val startPos = Position(grid.keys.map(_.x).max / 2, grid.keys.map(_.y).max / 2)

    accumulator(startPos, Delta(0, -1), 0, 0, grid.withDefaultValue('.'))
  }

  override def run(): Any = {
    val input: List[List[Char]] = Source.fromResource("day22.txt").getLines.toList.map(_.toList)
    spread(input, 10000000)
  }

}
