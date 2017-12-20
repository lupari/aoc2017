package challenge

import base.Challenge

import scala.io.Source

object Day20 extends Challenge {

  case class Coordinate(p: Long, v: Long, a: Long)

  case class Particle(id: Int, x: Coordinate, y: Coordinate, z: Coordinate) {
    def displacement(t: Int, c: Coordinate): Long = c.v * t + (0.5 * c.a * t * t).toLong

    def distance(t: Int): Long = List(x, y, z).map(c => c.p + displacement(t, c)).map(math.abs).sum
  }

  def parse(line: String, id: Int): Particle = {
    val pattern = "p=<([-0-9]+),([-0-9]+),([-0-9]+)>, v=<([-0-9]+),([-0-9]+),([-0-9]+)>, a=<([-0-9]+),([-0-9]+),([-0-9]+)>".r
    val pattern(xp, yp, zp, xv, yv, zv, xa, ya, za) = line
    val coords: (Coordinate, Coordinate, Coordinate) =
      (Coordinate(xp.toLong, xv.toLong, xa.toLong), Coordinate(yp.toLong, yv.toLong, ya.toLong), Coordinate(zp.toLong, zv.toLong, za.toLong))
    Particle(id, coords._1, coords._2, coords._3)
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day20.txt").getLines().toList
    val particles: List[Particle] = input.zipWithIndex.map(n => parse(n._1, n._2))
    particles.minBy(_.distance(500000)).id
  }

}
