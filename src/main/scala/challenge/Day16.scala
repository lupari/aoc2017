package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day16 extends Challenge {

  def spin(s: String, n: Int): String = s.takeRight(n) + s.take(s.length - n)

  def exchange(s: String, a: Int, b: Int): String = s.updated(a, s(b)).updated(b, s(a))

  def partner(s: String, a: Char, b: Char): String = exchange(s, s.indexOf(a.toString), s.indexOf(b.toString))

  def exec(instructions: List[String]): String = {

    @tailrec
    def accumulator(xs: List[String], acc: String): String = xs match {
      case h :: t => h match {
        case s if s.startsWith("s") => accumulator(t, spin(acc, s.drop(1).toInt))
        case s if s.startsWith("x") =>
          val parts = s.drop(1).split('/')
          accumulator(t, exchange(acc, parts.head.toInt, parts.last.toInt))
        case s if s.startsWith("p") =>
          val parts = s.drop(1).split('/')
          accumulator(t, partner(acc, parts.head.head, parts.last.last))
        case _ => throw new NoSuchMethodError
      }
      case Nil => acc
    }

    accumulator(instructions, "abcdefghijklmnop")
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day16.txt").getLines().mkString.split(',').toList
    exec(input)
  }

}
