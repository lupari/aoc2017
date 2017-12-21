package challenge

import base.Challenge

import scala.collection.mutable
import scala.io.Source

object Day21 extends Challenge {

  type Grid = Vector[String]
  type GridList = Vector[Grid]

  val memo: mutable.Map[Grid, Grid] = mutable.Map()

  def flip(grid: Grid): GridList = Vector(grid.reverse, grid.map(_.reverse))

  def rotate(grid: Grid): GridList = {
    def rotate90(l: Grid): Grid = l.transpose.map(_.reverse).map(_.mkString)

    (1 to 3).foldLeft(Vector(rotate90(grid)))((a, _) => a :+ rotate90(a.last))
  }

  def matches(grid: Grid): Set[Grid] = {
    val rotated: GridList = rotate(grid) :+ grid
    val flipped: GridList = rotated.flatMap(flip)
    (rotated ++ flipped).toSet
  }

  def divide(grid: Grid): GridList = {
    val n: Int = if (grid.size % 2 == 0) 2 else 3
    grid.grouped(n).map(g => g.map(s => s.grouped(n).toList).transpose).toVector.flatten
  }

  def join(grids: GridList): Grid = {
    val gs: Int = math.sqrt(grids.length.toDouble).toInt
    gs match {
      case 1 => grids.head
      case _ =>
        val ts: Int = grids.head.length
        grids.grouped(gs).map(g => (0 until ts).map(i => g.indices.map(j => g(j)(i)).mkString)).flatten.toVector
    }
  }

  def transform(grid: Grid, rules: Map[Grid, Grid]): Grid = {
    if (memo.contains(grid)) memo(grid)
    else {
      val rule: Grid = rules.keys.find(k => matches(grid).contains(k)).get
      val transformation = rules(rule)
      memo(grid) = transformation
      transformation
    }
  }

  def parse(line: String): (Grid, Grid) = {
    val parts = line.split(" => ")
    val rule = List(parts.head, parts.last).map(_.split("/").toVector.map(_.mkString))
    (rule.head, rule.last)
  }

  def execute(limit: Int): Int = {
    val input: Grid = Source.fromResource("day21.txt").getLines().toVector
    val transformations: Map[Grid, Grid] = input.map(parse).toMap
    val result: Grid = (1 to limit).zipWithIndex.foldLeft(Vector(".#.", "..#", "###"))((a, b) => {
      val replacements: GridList = divide(a).map(transform(_, transformations))
      // no need to join during the last round
      if (b._2 == limit -1) replacements.flatten else join(replacements)
    })
    result.map(_.count(_ == '#')).sum
  }

  override def run(): Any = execute(5)

}
