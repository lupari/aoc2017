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
    private val _condPart = args.split(" if ").last.split(" ")

    private val dest = args.takeWhile(c => !c.isWhitespace)
    private val n = _amount.toInt * _direction
    private val cond: (String, String, Int) = (_condPart(0), _condPart(1), _condPart(2).toInt)

    def exec(): Unit = cond match {
      case (r, "<", v) => if (registry(r)._1 < v) incRegistry(dest, n)
      case (r, ">", v) => if (registry(r)._1 > v) incRegistry(dest, n)
      case (r, "<=", v) => if (registry(r)._1 <= v) incRegistry(dest, n)
      case (r, ">=", v) => if (registry(r)._1 >= v) incRegistry(dest, n)
      case (r, "==", v) => if (registry(r)._1 == v) incRegistry(dest, n)
      case (r, "!=", v) => if (registry(r)._1 != v) incRegistry(dest, n)
      case _ => println("unknown cmd " + cond)
    }
  }

  override def run(): Unit = {
    val input: List[String] = Source.fromResource("day8.txt").getLines().toList
    val instructions: List[Inc] = input.map(Inc)
    instructions.foreach(i => i.exec())
    println(registry.values.map(v => v._2).max)
  }

}



