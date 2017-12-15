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

    val generatorA: Iterator[Int] = generator(a0, 16807)(_ % 4 == 0)
    val generatorB: Iterator[Int] = generator(b0, 48271)(_ % 8 == 0)

    (1 to limit).count(i => {
      val a = generatorA.next
      val b = generatorB.next
      zeroPad(a.toBinaryString).takeRight(16) == zeroPad(b.toBinaryString).takeRight(16)
    })

  }

  override def run(): Any = {
    matchCount(5000000, 591, 393)
  }

}
