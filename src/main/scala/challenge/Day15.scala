package challenge

import base.Challenge

object Day15 extends Challenge {

  def zeroPad(s: String): String = s match {
    case `s` if s.length < 16 => "0" * (16 - s.length) + s
    case _ => s
  }

  def ls16(x: Int): String = zeroPad(x.toBinaryString).takeRight(16)

  def matchCount(limit: Int, a0: Int, b0: Int): Int = {

    def generator(seed: Int, factor: Int): Iterator[Int] =
      Iterator.iterate(seed)(x => ((x.toLong * factor) % 2147483647).toInt).slice(1, limit + 1)

    generator(a0, 16807) zip generator(b0, 48271) count(g => ls16(g._1) == ls16(g._2))
  }

  override def run(): Any = {
    matchCount(40000000, 591, 393)
  }

}
