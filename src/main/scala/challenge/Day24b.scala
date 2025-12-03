package challenge

import base.Challenge
import challenge.Day24.{Component, dfs}

import scala.io.Source

object Day24b extends Challenge {

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day24.txt").getLines.toList
    val components: Set[(Int, Int)] = input.map(_.split("/").map(_.toInt)).map(x => (x.head, x.last)).toSet
    val paths: List[List[Component]] = dfs(components.toList)
    paths.map(p => (p.length, p.map(c => c.p1 + c.p2).sum)).max._2
  }


}
