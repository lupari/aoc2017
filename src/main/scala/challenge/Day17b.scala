package challenge

import base.Challenge

import scala.annotation.tailrec

object Day17b extends Challenge {

  def nextPos(pos: Int, n: Int, len: Int): Int = (n + pos) % len + 1

  def spinlock(steps: Int, limit: Int): Int = {

    @tailrec
    def sl(i: Int, pos: Int, acc: Int): Int = {
      val newPos = nextPos(pos, steps, i)
      i match {
        case `limit` => acc
        case _ => sl(i + 1, newPos, if (newPos == 1) i else acc)
      }
    }

    sl(1, 0, 0)
  }

  override def run(): Any = {
    spinlock(370, 50000000)
  }

}
