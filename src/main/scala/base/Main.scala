package base

import challenge._

import scala.collection.immutable.ListMap

object Main extends App {

  def challenges: Map[String, (Challenge, Any)] = ListMap(
    "1" -> ((Day1, 1049)),
    "1b" -> ((Day1b, 1508)),
    "2" -> ((Day2, 58975)),
    "2b" -> ((Day2b, 308)),
    "3" -> ((Day3, 480)),
    "3b" -> ((Day3b, 349975)),
    "4" -> ((Day4, 455)),
    "4b" -> ((Day4b, 186)),
    "5" -> ((Day5, 374269)),
    "5b" -> ((Day5b, 27720699)),
    "6" -> ((Day6, 7864)),
    "6b" -> ((Day6b, 1695)),
    "7" -> ((Day7, "airlri")),
    "7b" -> ((Day7b, 1206)),
    "8" -> ((Day8, 7787)),
    "8b" -> ((Day8b, 8997)),
    "9" -> ((Day9, 11846)),
    "9b" -> ((Day9b, 6285)),
    "10" -> ((Day10, 19591)),
    "10b" -> ((Day10b, "62e2204d2ca4f4924f6e7a80f1288786")),
    "11" -> ((Day11, 650)),
    "11b" -> ((Day11b, 1465)),
    "12" -> ((Day12, 141)),
    "12b" -> ((Day12b, 171)),
    "13" -> ((Day13, 632)),
    "13b" -> ((Day13b, 3849742)),
    "14" -> ((Day14, 8316)),
    "14b" -> ((Day14b, 1074)),
    "15" -> ((Day15, 619)),
    "15b" -> ((Day15b, 290)),
    "16" -> ((Day16, "padheomkgjfnblic")),
    "16b" -> ((Day16b, "bfcdeakhijmlgopn")),
    "17" -> ((Day17, 1244)),
    "17b" -> ((Day17b, 11162912)),
    "18" -> ((Day18, 1187)),
    "18b" -> ((Day18b, 5969)),
    "19" -> ((Day19, "GEPYAWTMLK")),
    "19b" -> ((Day19b, 17628)),
    "20" -> ((Day20, 308)),
    "20b" -> ((Day20b, 504)),
    "21" -> ((Day21, 133)),
    "21b" -> ((Day21b, 2221990)),
    "22" -> ((Day22, 5565)),
    "22b" -> ((Day22b, 2511978)),
    "23" -> ((Day23, 3969)),
    "23b" -> ((Day23b, 917)),
    "24" -> ((Day24, 1695)),
    "24b" -> ((Day24b, 1673)),
    "25" -> ((Day25, 3732))
  )

  def check(key: String): Unit = {
    val entry = challenges(key)
    val t0 = System.currentTimeMillis()
    val (result, expected) = (entry._1.run(), entry._2)
    val t1 = System.currentTimeMillis()
    println("Result for " + key + ":\t" + result + " (executed in " + (t1 - t0) + " ms)")
    assert(result == expected, "Bad test result for key " + key + ", expected " + expected + " but got " + result)
  }

  args.toList match {
    case h :: Nil => h match {
      case "all" => challenges.keys.foreach(check)
      case k if challenges.contains(k) => check(k)
      case _ => println("not found")
    }
    case _ => println("wrong number of args")
  }

}
