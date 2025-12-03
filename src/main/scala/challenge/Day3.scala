package challenge

import base.Challenge

object Day3 extends Challenge {

  def getDistance(x: Double): Int = {
    val _level = math.sqrt(x).ceil
    val level = if (_level % 2 == 0) _level + 1.0 else _level
    val max = math.pow(level, 2).toInt
    val min = math.pow(level - 2, 2).toInt
    val length = (max - min) / 4
    val axises = List(0, 1, 2, 3).map(max - _ * length - length / 2)
    axises.map(a => (x - a).abs).min.toInt + (level / 2).toInt
  }

  override def run(): Any = {
    getDistance(347991)
  }

}
