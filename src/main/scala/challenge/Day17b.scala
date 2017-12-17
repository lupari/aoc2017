package challenge

import base.Challenge

import scala.annotation.tailrec

object Day17b extends Challenge {

  def nextPos(pos: Int, n: Int, len: Int): Int = (n + pos) % len + 1

  def spinlock(steps: Int, limit: Int): Int = {

    @tailrec
    def sl(i: Int, pos: Int, acc: Int): Int = {
      val newPos = nextPos(pos, steps, i)
      if (i == limit) acc
      else if (newPos == 1) sl(i + 1, newPos, i)
      else sl(i + 1, newPos, acc)
    }

    sl(1, 0, 0)
  }

  override def run(): Any = {
    spinlock(370, 50000000)
  }

}
