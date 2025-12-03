package challenge

import base.Challenge

import scala.annotation.tailrec

object Day14b extends Challenge {

  case class Delta(x: Int, y: Int)

  case class Square(x: Int, y: Int, value: Char) {

    def +(delta: Delta): (Int, Int) = (x + delta.x, y + delta.y)

    def canMove(delta: Delta): Boolean = delta match {
      case Delta(0, -1) => y > 0
      case Delta(0, 1) => y < 127
      case Delta(-1, 0) => x > 0
      case Delta(1, 0) => x < 127
      case _ => false
    }

  }

  def dfs(start: Square, grid: List[List[Square]]): List[Square] = {

    def lookup(coords: (Int, Int)): Square = grid(coords._2)(coords._1)

    def visit(s: Square, visited: List[Square]): List[Square] = {
      if (visited.contains(s)) visited
      else {
        val neighbors: List[Square] = List(Delta(-1, 0), Delta(1, 0), Delta(0, -1), Delta(0, 1))
          .filter(d => s.canMove(d)).map(s + _)
          .map(lookup)
        val adjacent = neighbors filter (_.value == '1') filterNot visited.contains
        adjacent.foldLeft(s :: visited)((b, a) => visit(a, b))
      }
    }

    visit(start, List())
  }

  def findDisconnected(grid: List[List[Square]]): List[List[Square]] = {

    @tailrec
    def accumulate(notVisited: List[Square], acc: List[List[Square]]): List[List[Square]] = notVisited match {
      case Nil => acc
      case h :: _ =>
        val visited = dfs(h, grid)
        val remainder = notVisited.filterNot(visited.toSet)
        accumulate(remainder, acc :+ visited)
    }

    val notVisited: List[Square] = grid.flatMap(_.filter(_.value == '1'))
    accumulate(notVisited, List())
  }

  def zeroPad(s: String): String = s match {
    case `s` if s.length < 4 => "0" * (4 - s.length) + s
    case _ => s
  }

  override def run(): Any = {
    val input: List[List[Int]] = (0 to 127).map("ljoxqyyw-" + _).map(_.map(_.toInt).toList).toList
    val hashes: List[String] = input.map(Day10b.hash)
    val lines: List[String] = hashes.map(_.map(c => zeroPad(BigInt(c.toString, 16).toInt.toBinaryString)).mkString)
    val grid = lines.zipWithIndex.map(y => y._1.zipWithIndex.map(x => Square(x._2, y._2, x._1)).toList)
    findDisconnected(grid).length
  }

}
