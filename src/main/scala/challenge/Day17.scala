package challenge

import base.Challenge

object Day17 extends Challenge {

  def spin(xs: List[Int], pos: Int, n: Int): (Int, List[Int]) = {
     val newPos = (n + pos) % xs.length
     (newPos + 1, xs.take(newPos + 1) ++ List(xs.length) ++ xs.takeRight(xs.length - newPos -1))
  }

  def spinLock(steps: Int, times: Int): List[Int] =
    (1 to times).foldLeft((0, List(0)))((i, s) => spin(i._2, i._1, steps))._2

  override def run(): Any = {
    val values = spinLock(370, 2017)
    values(values.indexOf(2017) + 1)
  }

}
