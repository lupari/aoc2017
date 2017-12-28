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

  def contains(components: List[Component], ports: (Int, Int)): Boolean = {
    val a = components.map(c => (c.p1, c.p2))
    a.contains(ports) || a.contains((ports._2, ports._1))
  }

  def dfs(components: List[(Int, Int)]): List[List[Component]] = {

    def visit(component: Component, visited: List[Component], acc: List[List[Component]]): List[List[Component]] = {
      val adjacent = components.filterNot(contains(visited, _)).map(component.bind).filter(_.isDefined).map(_.get)
      if (adjacent.isEmpty) acc :+ visited
      else adjacent.flatMap(a => visit(a, visited :+ a, acc))
    }

    val start = Component(0, 0, 0)
    visit(start, List(start), List())
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day24.txt").getLines.toList
    val components: Set[(Int, Int)] = input.map(_.split("/").map(_.toInt)).map(x => (x.head, x.last)).toSet
    val paths: List[List[Component]] = dfs(components.toList)
    paths.map(l => l.map(c => c.p1 + c.p2).sum).max
  }


}
