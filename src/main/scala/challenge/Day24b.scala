package challenge

import base.Challenge
import challenge.Day24.{Bridge, allBridges}

import scala.io.Source

object Day24b extends Challenge {

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day24.txt").getLines.toList
    val components: Set[(Int, Int)] = input.map(_.split("/").map(_.toInt)).map(x => (x.head, x.last)).toSet
    allBridges(components, Bridge(Nil, 0)).map(b => (b.components.length, b.components.map(c => c._1 + c._2).sum)).max._2
  }


}
