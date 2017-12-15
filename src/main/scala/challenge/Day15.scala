package challenge

import base.Challenge

object Day15 extends Challenge {

  def zeroPad(s: String): String = s match {
    case `s` if s.length < 16 => "0" * (16 - s.length) + s
    case _ => s
  }

  def generate(seed: Int, factor: Int): Stream[Long] = {
    lazy val generator: Stream[Long] = seed #:: generator.map {x => (x * factor) % 2147483647}
    generator
  }

  def matchCount(limit: Int, a0: Int, b0: Int): Int = {

    val generatorA: Iterator[Long] = generate(a0, 16807).slice(1, limit + 1).iterator
    val generatorB: Iterator[Long] = generate(b0, 48271).slice(1, limit + 1).iterator

    (1 to limit).count(i => {
      val a = generatorA.next()
      val b = generatorB.next()
      zeroPad(a.toBinaryString).takeRight(16) == zeroPad(b.toBinaryString).takeRight(16)
    })

  }

  override def run(): Any = {
    matchCount(40000000, 591, 393)
  }

}
