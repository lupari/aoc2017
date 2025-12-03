package challenge

import base.Challenge

import scala.io.Source

object Day7b extends Challenge {


  case class Program(name: String, weight: Int, childList: String) {
    var children: List[Program] = Nil
    var totalWeight: Int = 0

    def initChildren(xs: Map[String, Program]): Unit = {
      children = if (childList == "") List() else childList.split(", ").map(xs(_)).toList
    }

    def withTotalWeight(w: Int): Program = {
      totalWeight = w
      this
    }

    def getTotalWeight: Int = totalWeight

    def getChildren: List[Program] = children

  }

  def buildTree(input: List[String]): Program = {

    def loadNodes(program: Program, registry: Map[String, Program]): Program = {
      program.initChildren(registry)
      for (c <- program.getChildren) yield loadNodes(c, registry)
      program
    }

    def parse(s: String): Program = {
      val name = s.takeWhile(_ != ' ')
      val weight = "([0-9]+)".r.findFirstIn(s).get.toInt
      val children: String = if (s.contains(" -> ")) s.split(" -> ").last else ""
      Program(name, weight, children)
    }

    val programs: List[Program] = input.map(parse)
    val childNames: Set[String] = programs.map(_.childList).flatMap(_.split(", ").toList).toSet
    val allNames = programs.map(_.name).toSet
    val rootName: String = allNames.diff(childNames).toSeq.head
    val registry: Map[String, Program] = programs.map(p => (p.name, p)).toMap
    loadNodes(registry(rootName), registry)
  }

  def weigh(program: Program): Program = program match {
    case p if p.getChildren.isEmpty =>
      p.withTotalWeight(program.weight)
    case p =>
      p.withTotalWeight(p.weight + p.getChildren.map(weigh(_).getTotalWeight).sum)
  }


  def findImbalanced(program: Program): Option[Program] = {
    val stacks = program.getChildren.groupBy(c => c.getTotalWeight)
    if (stacks.size == 1) {
      Option.empty
    } else {
      val parent: Program = stacks.find(_._2.size == 1).get._2.head
      val badChild = findImbalanced(parent)
      if (badChild.isDefined) {
        badChild
      } else {
        val siblingWeight = stacks.find(_._2.size > 1).get._2.head.getTotalWeight
        val diff = siblingWeight - parent.getTotalWeight + parent.weight
        Option(Program(parent.name, diff, ""))
      }
    }
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day7.txt").getLines().toList
    val tree = buildTree(input)
    weigh(tree)
    findImbalanced(tree).get.weight

  }

}
