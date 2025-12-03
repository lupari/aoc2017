package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day13b extends Challenge {

  case class Layer(range: Int) {
    def costAt(t: Int): Int = range match {
      case 0 => 0
      case r if t % ((r - 1) * 2) == 0 => t * r
      case _ => 0
    }
  }

  @tailrec
  def findWaitTime(t: Int, layers: List[Layer]): Int =
    if (layers.zipWithIndex.forall(x => x._1.costAt(t + x._2) == 0)) t
    else findWaitTime(t + 1, layers)

  override def run(): Any = {
    val input: Map[Int, Int] = Source.fromResource("day13.txt").getLines()
      .map(_.split(": "))
      .map(a => (a.head.toInt, a.last.toInt)).toMap
      .withDefaultValue(0)

    val layers = for (i <- 0 to input.keys.max) yield Layer(input(i))

    findWaitTime(0, layers.toList)

  }

}
