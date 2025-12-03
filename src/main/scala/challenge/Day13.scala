package challenge

import base.Challenge

import scala.io.Source

object Day13 extends Challenge {

  case class Layer(range: Int) {
    def costAt(t: Int): Int = range match {
      case 0 => 0
      case r if t % ((r - 1) * 2) == 0 => t * r
      case _ => 0
    }
  }

  override def run(): Any = {
    val input: Map[Int, Int] = Source.fromResource("day13.txt").getLines()
      .map(s => s.split(": "))
      .map(a => (a.head.toInt, a.last.toInt)).toMap
      .withDefaultValue(0)

    val layers = for (i <- 0 to input.keys.max) yield Layer(input(i))
    layers.zipWithIndex.foldLeft(0)((a, b) => a + b._1.costAt(b._2))
  }

}
