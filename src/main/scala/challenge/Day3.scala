package challenge

import base.Challenge

object Day3 extends Challenge {

  def getDistance(x: Int): Int = {
    val _level = math.sqrt(x).ceil.toInt
    val level = if (_level % 2 == 0) _level + 1 else _level
    val max = math.pow(level, 2).toInt
    val min = math.pow(level - 2, 2).toInt
    val length = (max - min) / 4
    val axises = List(0, 1, 2, 3).map(max - _ * length - length / 2)
    axises.map(a => math.abs(x - a)).min + level / 2
  }

  override def run(): Any = {
    getDistance(347991)
  }

}
