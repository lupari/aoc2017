package challenge

import base.Challenge

import scala.io.Source

object Day24 extends Challenge {

  case class Component(p1: Int, p2: Int, ending: Int) {
    def bind(ports: (Int, Int)): Option[Component] = ports match {
      case (a, b) if a == ending => Some(Component(a, b, b))
      case (a, b) if b == ending => Some(Component(a, b, a))
      case _ => None
    }
  }

  def contains(components: List[Component], ports: (Int, Int)): Boolean =
    components.exists(c => (c.p1, c.p2) == ports || (c.p2, c.p1) == ports)

  def dfs(components: List[(Int, Int)]): List[List[Component]] = {

    def visit(visited: List[Component]): List[List[Component]] = {
      val adjacent = components.filterNot(contains(visited, _)).flatMap(visited.last.bind)
      if (adjacent.isEmpty) List(visited)
      else adjacent.flatMap(a => visit(visited :+ a))
    }

    visit(List(Component(0, 0, 0)))
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day24.txt").getLines.toList
    val components: Set[(Int, Int)] = input.map(_.split("/").map(_.toInt)).map(x => (x.head, x.last)).toSet
    val paths: List[List[Component]] = dfs(components.toList)
    paths.map(l => l.map(c => c.p1 + c.p2).sum).max
  }


}
