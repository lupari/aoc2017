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

    var mu = 0
    tortoise = x0
    while (tortoise != hare) {
      tortoise = f(tortoise)
      hare = f(hare)
      mu += 1
    }

    var lambda = 1
    hare = f(tortoise)
    while (tortoise != hare) {
      hare = f(hare)
      lambda += 1
    }

    (mu, lambda)
  }

  def dance(moves: List[String]): Vector[Char] = {
    val programs = ('a' to 'p').toVector
    val (mu, lambda) = floyd(programs, (items: Vector[Char]) => exec(moves, items))
    (0 until (1000000000 - mu) % lambda).foldLeft(programs)((programs, _) => exec(moves, programs).toVector)
  }

  override def run(): Any = {
    val input: List[String] = Source.fromResource("day16.txt").getLines().mkString.split(',').toList
    dance(input).mkString
  }

}
