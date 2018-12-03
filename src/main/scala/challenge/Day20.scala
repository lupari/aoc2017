package challenge

import base.Challenge

import scala.io.Source

object Day20 extends Challenge {

  case class Coordinate(p: Long, v: Long, a: Long)

  case class Particle(id: Int, x: Coordinate, y: Coordinate, z: Coordinate) {
    def displacement(t: Int, c: Coordinate): Long = c.v * t + (0.5 * c.a * t * t).toLong

    def distance(t: Int): Long = List(x, y, z).map(c => c.p + displacement(t, c)).map(_.abs).sum
  }

  def parse(line: String, id: Int): Particle = {
    val pattern = "p=<([-0-9]+),([-0-9]+),([-0-9]+)>, v=<([-0-9]+),([-0-9]+),([-0-9]+)>, a=<([-0-9]+),([-0-9]+),([-0-9]+)>".r
    val pattern(xp, yp, zp, xv, yv, zv, xa, ya, za) = line
    val coords: Seq[Coordinate] = Seq(Seq(xp, xv, xa), Seq(yp, yv, ya), Seq(zp, zv, za)).map(_.map(_.toLong))
      .map(s => Coordinate(s.head, s(1), s.last))
    Particle(id, coords.head, coords(1), coords.last)
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day20.txt").getLines().toList
    val particles: List[Particle] = input.zipWithIndex.map(n => parse(n._1, n._2))
    particles.minBy(_.distance(500000)).id
  }

}
