package challenge

import base.Challenge

import scala.annotation.tailrec

object Day10b extends Challenge {

  def hash(input: List[Int]): String = {

    def createHash(xs: List[Int], lengths: List[Int], pos: Int, skip: Int): (List[Int], Int, Int) = {

      def hash(xs: Vector[Int], pos: Int, l: Int): Vector[Int] = l match {
        case x if pos + x >= xs.length =>
          val overflow = pos + x - xs.length
          val underflow = xs.length - pos
          val seg = (xs.takeRight(underflow) ++ xs.take(overflow)).reverse
          seg.takeRight(overflow) ++ xs.slice(overflow, xs.length - underflow) ++ seg.take(underflow)
        case _ => xs.take(pos) ++ xs.slice(pos, pos + l).reverse ++ xs.takeRight(xs.length - pos - l)
      }

      @tailrec
      def accumulator(xs: Vector[Int], lengths: List[Int], pos: Int, skip: Int): (List[Int], Int, Int) = lengths match {
        case h :: t => accumulator(hash(xs, pos, h), t, (pos + h + skip) % xs.length, skip + 1)
        case _ => (xs.toList, pos, skip)
      }

      accumulator(xs.toVector, lengths, pos, skip)
    }

    @tailrec
    def sparseHash(xs: List[Int], lengths: List[Int], pos: Int, skip: Int, i: Int): List[Int] = i match {
      case `i` if i > 0 =>
        val h = createHash(xs, lengths, pos, skip)
        sparseHash(h._1, lengths, h._2, h._3, i - 1)
      case _ => xs
    }

    val stream = 0 to 255
    val sparse = sparseHash(stream.toList, input  ++ List(17, 31, 73, 47, 23), 0, 0, 64)
    val dense = sparse.grouped(16).map(_.reduceRight(_ ^ _)).toList
    dense.map(_.toHexString).map(c => if (c.length == 1) '0' + c else c).mkString
  }

  override def run(): Any = {
    val input: List[Int] = "129,154,49,198,200,133,97,254,41,6,2,1,255,0,191,108".toList.map(_.toInt)
    hash(input)
  }

}
