package challenge

import base.Challenge

object Day15b extends Challenge {

  def zeroPad(s: String): String = s match {
    case `s` if s.length < 16 => "0" * (16 - s.length) + s
    case _ => s
  }

  def ls16(x: Int): String = zeroPad(x.toBinaryString).takeRight(16)

  def matchCount(limit: Int, a0: Int, b0: Int): Int = {

    def generator(seed: Int, factor: Int)(cond: Int => Boolean): Iterator[Int] =
      Iterator.iterate(seed)(x => ((x.toLong * factor) % 2147483647).toInt)
        .drop(1).filter(cond).take(limit)

    generator(a0, 16807)(_ % 4 == 0) zip generator(b0, 48271)(_ % 8 == 0) count(g => ls16(g._1) == ls16(g._2))
  }

  override def run(): Any = {
    matchCount(5000000, 591, 393)
  }

}
