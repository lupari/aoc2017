package challenge

import base.Challenge

import scala.collection.mutable
import scala.io.Source

object Day8b extends Challenge {

  val registry: mutable.Map[String, (Int, Int)] = mutable.Map[String, (Int, Int)]().withDefaultValue((0, 0))

  def incRegistry(k: String, v: Int) {
    val status = registry(k)
    val updated = status._1 + v
    registry(k) = (updated, if (updated > status._2) updated else status._2)
  }

  case class Inc(args: String) {

    private val _amount = args.dropWhile(c => !c.isDigit && !c.equals('-')).takeWhile(c => c.isDigit || c.equals('-'))
    private val _direction = if (args.contains("inc")) 1 else -1

    private val cond = args.split(" if ").last.split(" ")
    private val dest = args.takeWhile(!_.isWhitespace)
    private val n = _amount.toInt * _direction

    private val condFn = cond(1) match {
      case "<" => (r: Int, v: Int) => {r < v}
      case ">" => (r: Int, v: Int) => {r > v}
      case "<=" => (r: Int, v: Int) => {r <= v}
      case ">=" => (r: Int, v: Int) => {r >= v}
      case "==" => (r: Int, v: Int) => {r == v}
      case "!=" => (r: Int, v: Int) => {r != v}
    }

    def exec(): Unit = if (condFn(registry(cond(0))._1, cond(2).toInt)) incRegistry(dest, n)

  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day8.txt").getLines().toList
    input.map(Inc).foreach(_.exec())
    registry.values.map(v => v._2).max
  }

}



