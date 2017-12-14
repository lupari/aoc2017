package base

import challenge._



object Main extends App {

  def challenges: Map[String, (() => Any, Any)] = Map(
    "1" -> (() => Day1.run(), 1049),
    "1b" -> (() => Day1b.run(), 1508),
    "2" -> (() => Day2.run(), 58975),
    "2b" -> (() => Day2b.run(), 308),
    "3" -> (() => Day3.run(), 480),
    "3b" -> (() => Day3b.run(), 349975),
    "4" -> (() => Day4.run(), 455),
    "4b" -> (() => Day4b.run(), 186),
    "5" -> (() => Day5.run(), 374269),
    "5b" -> (() => Day5b.run(), 27720699),
    "6" -> (() => Day6.run(), 7864),
    "6b" -> (() => Day6b.run(), 1695),
    "7" -> (() => Day7.run(), "airlri"),
    "7b" -> (() => Day7b.run(), 1206),
    "8" -> (() => Day8.run(), 7787),
    "8b" -> (() => Day8b.run(), 8997),
    "9" -> (() => Day9.run(), 11846),
    "9b" -> (() => Day9b.run(), 6285),
    "10" -> (() => Day10.run(), 19591),
    "10b" -> (() => Day10b.run(), "62e2204d2ca4f4924f6e7a80f1288786"),
    "11" -> (() => Day11.run(), 650),
    "11b" -> (() => Day11b.run(), 1465),
    "12" -> (() => Day12.run(), 141),
    "12b" -> (() => Day12b.run(), 171),
    "13" -> (() => Day13.run(), 632),
    "13b" -> (() => Day13b.run(), 3849742),
    "14" -> (() => Day14.run(), 8316),
    "14b" -> (() => Day14b.run(), 1074),
  )

  def check(key: String): Unit = {
    val entry = challenges(key)
    val (result, expected) = (entry._1(), entry._2)
    println("result for " + key + " = " + result)
    assert(result == expected, "Bad test result for key " + key + ", expected " + expected + " but got " + result)
  }

  if (args.length != 1) {
    println("wrong number of args")
  } else {
    args(0) match {
      case "all" => challenges.keys.foreach(check)
      case k if challenges.contains(k) => check(k)
      case _ => println("not found")
    }
  }


}
