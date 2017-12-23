package challenge

import base.Challenge

object Day23b extends Challenge {

  def isPrime(i: Int): Boolean = {
    if (i <= 1) false
    else if (i == 2) true
    else !(2 until i).exists(x => i % x == 0)
  }

  override def run(): Any = {
    106500.to(123500).by(17).count(!isPrime(_))
  }

}
