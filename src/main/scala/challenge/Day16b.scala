package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.io.Source

object Day16b extends Challenge {

  def spin(s: String, n: Int): String = s.takeRight(n) + s.take(s.length - n)

  def exchange(s: String, a: Int, b: Int) = {
    val (x, y) = (s(a), s(b))
    s.updated(a, y).updated(b, x)
  }

  def partner(s: String, a: Char, b: Char) = {
    exchange(s, s.indexOf(a.toString), s.indexOf(b.toString))
  }

  def exec(instructions: List[String], seed: Vector[Char]): Vector[Char] = {

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

    accumulator(instructions, seed.mkString).toVector
  }

  def floyd[A](x0: A, f: A => A): (Int, Int) = {
    var tortoise = f(x0)
    var hare = f(f(x0))
    while (tortoise != hare) {
      tortoise = f(tortoise)
      hare = f(f(hare))
    }

    var μ = 0
    tortoise = x0
    while (tortoise != hare) {
      tortoise = f(tortoise)
      hare = f(hare)
      μ += 1
    }

    var λ = 1
    hare = f(tortoise)
    while (tortoise != hare) {
      hare = f(hare)
      λ += 1
    }

    (μ, λ)
  }

  def dance(moves: List[String]): Vector[Char] = {
    val programs = ('a' to 'p').toVector
    val (μ, λ) = floyd(programs, (items: Vector[Char]) => exec(moves, items))
    (0 until (1000000000 - μ) % λ).foldLeft(programs)((programs, _) => exec(moves, programs).toVector)
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day16.txt").getLines().mkString.split(',').toList
    dance(input).mkString
  }

}
