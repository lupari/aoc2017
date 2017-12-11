package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day11b extends Challenge {

  case class Delta(x: Int, y: Int, name: String)
  case class Point(x: Int, y: Int) {
    def +(delta: Delta): Point = Point(x + delta.x, y + delta.y)
    def distance(p: Point): Int = {
      List(math.abs(x - p.x), math.abs(y - p.y)).max
    }
  }
  def X = Point(0, 0)
  def N = Delta(0, 1, "n")
  def NE = Delta(1, 1, "ne")
  def SE = Delta(1, -1, "se")
  def S = Delta(0, -1, "s")
  def SW = Delta(-1, -1, "sw")
  def NW = Delta(-1, 1, "nw")

  def DIRECTIONS: Map[String, Delta] = List(N, NE, SE, S, SW, NW).map(d => (d.name, d)).toMap

  def locateFarthest(xs: List[String]): Point = {

    def getFarthest(p: Point, farthest: Point): Point = if (X.distance(p) > X.distance(farthest)) p else farthest

    @tailrec
    def accumulator(xs: List[String], farthest: Point, acc: Point): Point = xs match {
      case h :: t if DIRECTIONS.contains(h) =>
        val dir = DIRECTIONS(h)
        accumulator(t, getFarthest(acc + dir, farthest), acc + dir)
      case _ => farthest
    }

    accumulator(xs, X, X)
  }

  override def run(): Unit = {
    val input = Source.fromResource("day11.txt").getLines().mkString.split(',').toList
    println(X.distance(locateFarthest(input)))
  }

}
