package challenge

import base.Challenge

import scala.io.Source

object Day12 extends Challenge {

  case class Vertex(lbl: Int)
  case class Graph(vertices:Set[Vertex], edges:Set[(Vertex,Vertex)])

  def parseEdges(input: String, vertices: Map[Int, Vertex]): List[(Vertex, Vertex)] = {
    val labels: List[Int]= input.split(" <-> ").last.split(", ").map(_.toInt).toList
    val src: Vertex = vertices(input.takeWhile(!_.isWhitespace).toInt)
    labels.map(l => (src, vertices(l)))
  }

  def dfs(start: Vertex, graph: Graph): List[Vertex] = {

    def findAdjacent(v: Vertex): List[Vertex] = graph.edges.filter(_._1.lbl == v.lbl).map(_._2).toList

    def visit(v: Vertex, visited: List[Vertex]): List[Vertex] = {
      if (visited.contains(v)) visited
      else {
        val adjacent = findAdjacent(v) filterNot visited.contains
        adjacent.foldLeft(v :: visited)((b, a) => visit(a, b))
      }
    }

    visit(start, List())
  }

  override def run(): Unit = {
    val input: List[String] = Source.fromResource("day12.txt").getLines().toList
    val vertices: Map[Int, Vertex] = input.map(_.takeWhile(c => !c.isWhitespace))
      .map(i => (i.toInt, Vertex(i.toInt))).toMap
    val edges: List[(Vertex, Vertex)] = input.flatMap(i => parseEdges(i, vertices))
    val graph: Graph = Graph(vertices.values.toSet, edges.toSet)
    println(dfs(vertices(0), graph).length)
  }


}
