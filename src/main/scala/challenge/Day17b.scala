package challenge

import base.Challenge

import scala.annotation.tailrec

object Day17b extends Challenge {

  def nextPos(pos: Int, n: Int, len: Int): Int = (n + pos) % len + 1

  def spinLock(steps: Int, limit: Int): Int = {

    @tailrec
    def accumulator(i: Int, pos: Int, acc: Int): Int = {
      val newPos = nextPos(pos, steps, i)
      i match {
        case `limit` => acc
        case _ => accumulator(i + 1, newPos, if (newPos == 1) i else acc)
      }
    }

    accumulator(1, 0, 0)
  }

  override def run(): Any = {
    spinLock(370, 50000000)
  }

}
