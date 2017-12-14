package challenge

import base.Challenge

import scala.io.Source

object Day7 extends Challenge {

  case class Node(name: String, children: Seq[String])

  def parse(s: String): Node = {
    val name = s.takeWhile(c => c != ' ')
    val children = if (s.contains(" -> ")) s.split(" -> ").last.split(", ").toSeq else Seq()
    Node(name, children)
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day7.txt").getLines().toList
    val mappings = input.map(parse)
    val children: Set[String] = mappings.flatMap(_.children).toSet
    val all = mappings.map(_.name).toSet
    all.diff(children).toSeq.head
  }

}



