package challenge

import base.Challenge

object Day15 extends Challenge {

  def zeroPad(s: String): String = s match {
    case `s` if s.length < 16 => "0" * (16 - s.length) + s
    case _ => s
  }

  def matchCount(limit: Int, a0: Int, b0: Int): Int = {

    def generator(seed: Int, factor: Int): Iterator[Int] = {
      lazy val stream: Stream[Int] = seed #:: stream.map(x => ((x.toLong * factor) % 2147483647).toInt)
      stream.drop(1).take(limit).iterator
    }

    def lsb(x: Int): String = zeroPad(x.toBinaryString).takeRight(16)

    generator(a0, 16807) zip generator(b0, 48271) count(g => lsb(g._1) == lsb(g._2))
  }

  override def run(): Any = {
    matchCount(40000000, 591, 393)
  }

}
