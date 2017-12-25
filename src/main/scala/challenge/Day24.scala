package challenge

import base.Challenge

import scala.io.Source

object Day24 extends Challenge {

  case class Bridge(components: List[(Int, Int)], last: Int) {
    def bind(c: (Int, Int)): Option[Bridge] = c match {
      case (p1, p2) if p1 == last => Some(Bridge(c :: components, p2))
      case (p1, p2) if p2 == last => Some(Bridge(c :: components, p1))
      case _ => None
    }
  }

  def allBridges(components: Set[(Int, Int)], acc: Bridge): Iterator[Bridge] = {
    val bridges = components.toIterator.flatMap(acc.bind)
    if (bridges.isEmpty) Iterator(acc)
    else bridges.flatMap(b => allBridges(components - b.components.head, b))
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day24.txt").getLines.toList
    val components: Set[(Int, Int)] = input.map(_.split("/").map(_.toInt)).map(x => (x.head, x.last)).toSet
    allBridges(components, Bridge(Nil, 0)).map(_.components.map(x => x._1 + x._2)).map(_.sum).max
  }


}
