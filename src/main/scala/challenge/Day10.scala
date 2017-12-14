package challenge

import base.Challenge

import scala.annotation.tailrec

object Day10 extends Challenge {

  def createHash(xs: List[Int], lengths: List[Int]): List[Int] = {

    def hash(xs: Vector[Int], pos: Int, l: Int): Vector[Int] = l match {
      case x if pos + x >= xs.length =>
        val overflow = pos + x - xs.length
        val underflow = xs.length - pos
        val seg = (xs.takeRight(underflow) ++ xs.take(overflow)).reverse
        seg.takeRight(overflow) ++ xs.slice(overflow, xs.length - underflow) ++ seg.take(underflow)
      case _ => xs.take(pos) ++ xs.slice(pos, pos + l).reverse ++ xs.takeRight(xs.length - pos - l)
    }

    @tailrec
    def accumulator(xs: Vector[Int], lengths: List[Int], pos: Int, skip: Int): List[Int] = lengths match {
      case h :: t => accumulator(hash(xs, pos, h), t, (pos + h + skip) % xs.length, skip + 1)
      case _ => xs.toList
    }

    accumulator(xs.toVector, lengths, 0, 0)
  }

  override def run(): Any = {
    val stream = 0 to 255
    val input = List(129, 154, 49, 198, 200, 133, 97, 254, 41, 6, 2, 1, 255, 0, 191, 108).filter(_ <= 255)
    val hash = createHash(stream.toList, input)
    hash.take(2).product
  }

}
