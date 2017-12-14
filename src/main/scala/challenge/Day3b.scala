package challenge

import base.Challenge

import scala.annotation.tailrec

object Day3b extends Challenge {

  def getIncrement(x: Int, size: Int, min: Int): Int = {
    List(0, 1, 2, 3).find(a => x >= min + (a * size) &&
      x < min + (a + 1) * size).get * 2
  }

  def getCategory(x: Int, size: Int, min: Int, max: Int): String = x match {
    case `x` if x == min => "F" // First of level
    case `x` if x == min + 1 => "S" // Second of level
    case `x` if x == max - 1 => "BL" // Before last
    case `x` if x == max => "L" // Last
    case `x` if List(0, 1, 2, 3).map(max - _ * size).contains(x) => "C" // Corner
    case `x` if List(0, 1, 2, 3).map(max - _ * size - 1).contains(x) => "BC" // Before corner
    case `x` if List(0, 1, 2, 3).map(max - _ * size + 1).contains(x) => "AC" // After corner
    case _ => "O" // Other
  }

  def getNeighbors(x: Int, diff: Int, category: String): List[Int] = category match {
    case "F" => List(x - diff + 1)
    case "S" => List(x - 2, x - diff, x - diff + 1)
    case "O" => List(x - diff, x - diff - 1, x - diff + 1)
    case "C" => List(x - diff - 1)
    case "AC" => List(x - 2, x - diff, x - diff + 1)
    case "BC" => List(x - diff, x - diff - 1)
    case "L" => List(x - diff, x - diff - 1)
    case "BL" => List(x - diff - 1, x - diff, x - diff + 1)
  }

  def getSum(xs: List[Int]): Int = {
    val pos = xs.length
    val _level = math.sqrt(pos).floor.toInt
    val level = if (_level % 2 == 0) _level - 1 else _level
    val max = math.pow(level + 2, 2).toInt - 1
    val min = math.pow(level, 2).toInt
    val length = (max - min) / 4 + 1
    val category = getCategory(pos, length, min, max)
    val diffLevel = ((level + 2) / 3) * 8 + 1
    val diffIncrement = getIncrement(pos, length, min)
    val diff = diffLevel + diffIncrement
    val neighbors = getNeighbors(pos, diff, category) :+ pos - 1
    neighbors.map(xs(_)).sum
  }

  def findFirstAbove(limit: Int): Int = {

    @tailrec
    def acc(xs: List[Int]): Int = if (xs.last > limit) xs.last else acc(xs :+ getSum(xs))

    acc(List(1, 1, 2, 4, 5, 10, 11, 23, 25))
  }

  override def run(): Any = {
    findFirstAbove(347991)
  }

}

