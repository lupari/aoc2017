package challenge

import base.Challenge

object Day15b extends Challenge {

  def zeroPad(s: String): String = s match {
    case `s` if s.length < 16 => "0" * (16 - s.length) + s
    case _ => s
  }

  def matchCount(limit: Int, a0: Int, b0: Int): Int = {

    def generator(seed: Int, factor: Int)(cond: Int => Boolean): Iterator[Int] = {
      lazy val stream: Stream[Int] = seed #:: stream.map(x => ((x.toLong * factor) % 2147483647).toInt)
      stream.drop(1).filter(cond).take(limit).iterator
    }

    def lsb(x: Int): String = zeroPad(x.toBinaryString).takeRight(16)

    generator(a0, 16807)(_ % 4 == 0) zip generator(b0, 48271)(_ % 8 == 0) count(g => lsb(g._1) == lsb(g._2))
  }

  override def run(): Any = {
    matchCount(5000000, 591, 393)
  }

}
