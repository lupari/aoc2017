package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day19 extends Challenge {

  case class Delta(x: Int, y: Int) {
    def opposite: Delta = Delta(x * -1, y * -1)
  }

  object Delta {
    def none: Delta = Delta(0, 0)
  }

  case class Square(x: Int, y: Int, value: Char) {

    def +(delta: Delta): (Int, Int) = (x + delta.x, y + delta.y)

    def inside(grid: List[List[Square]], d: Delta): Boolean =
      grid.head.indices.contains(x + d.x) && grid.indices.contains(y + d.y)

    def canMove(grid: List[List[Square]], delta: Delta): Boolean = !grid(y + delta.y)(x + delta.x).value.isWhitespace

    def nextMove(grid: List[List[Square]], prev: Delta): Delta = {
      val moves = List(Delta(0, -1), Delta(0, 1), Delta(-1, 0), Delta(1, 0))
      val possible: List[Delta] = moves.filterNot(_ == prev.opposite).filter(d => inside(grid, d) && canMove(grid, d))
      possible match {
        case Nil => Delta(0, 0)
        case l if l.contains(prev) => prev
        case _ => possible.head
      }
    }

  }

  def travel(grid: List[List[Square]]): List[(Square)] = {

    def lookup(coords: (Int, Int)): Square = grid(coords._2)(coords._1)

    @tailrec
    def accumulator(acc: List[(Square, Delta)]): List[Square] = {
      val (square, prev) = acc.last
      val next = square.nextMove(grid, prev)
      next match {
        case Delta(0, 0) => acc.map(_._1)
        case d => accumulator(acc :+ ((lookup(square + d), d)))
      }
    }

    val start: Square = grid.head.find(s => List('|', '-').contains(s.value)).get
    accumulator(List((start, Delta.none)))
  }

  override def run(): Any = {
    val input: List[List[Char]] = Source.fromResource("day19.txt").getLines().toList.map(_.toList)
    val maxLineWidth = input.map(_.length).max
    val wideInput = input.map(l => l ++ List.fill(maxLineWidth - l.length)(' '))
    val grid = wideInput.zipWithIndex.map(y => y._1.zipWithIndex.map(x => Square(x._2, y._2, x._1)))
    val path: List[Square] = travel(grid)
    path.map(_.value).filter(_.isLetter).mkString
  }

}
