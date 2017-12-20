package challenge

import base.Challenge

import scala.io.Source
import scala.util.matching.Regex

object Day20b extends Challenge {

  case class Coordinate(p: Long, v: Long, a: Long)

  case class Particle(x: Coordinate, y: Coordinate, z: Coordinate) {
    def advance(): Particle = {
      val xv = x.v + x.a
      val yv = y.v + y.a
      val zv = z.v + z.a
      val xp = x.p + xv
      val yp = y.p + yv
      val zp = z.p + zv
      Particle(Coordinate(xp, xv, x.a), Coordinate(yp, yv, y.a), Coordinate(zp, zv, z.a))
    }
  }

  val pattern: Regex = "p=<([-0-9]+),([-0-9]+),([-0-9]+)>, v=<([-0-9]+),([-0-9]+),([-0-9]+)>, a=<([-0-9]+),([-0-9]+),([-0-9]+)>".r

  def parse(line: String): Particle = {
    val pattern(xp, yp, zp, xv, yv, zv, xa, ya, za) = line
    val coords: Seq[Coordinate] = Seq(Seq(xp, xv, xa), Seq(yp, yv, ya), Seq(zp, zv, za)).map(_.map(_.toLong))
      .map(x => Coordinate(x.head, x(1), x.last))
    Particle(coords.head, coords(1), coords.last)
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day20.txt").getLines().toList
    val particles: List[Particle] = input.map(parse)
    (0 to 10000).foldLeft(particles)((acc, _) => {
      val groups = acc.groupBy(p => (p.x.p, p.y.p, p.z.p))
      groups.values.filter(_.length == 1).flatten.toList.map(_.advance())
    }).length
  }

}
