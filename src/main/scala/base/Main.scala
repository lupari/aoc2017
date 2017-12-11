package base

import challenge._

object Main extends App {

  if (args.length != 1) {
    println("wrong number of args")
  } else {
    args(0) match  {
      case "day1" => Day1.run()
      case "day1b" => Day1b.run()
      case "day2" => Day2.run()
      case "day2b" => Day2b.run()
      case "day3" => Day3.run()
      case "day3b" => Day3b.run()
      case "day4" => Day4.run()
      case "day4b" => Day4b.run()
      case "day5" => Day5.run()
      case "day5b" => Day5b.run()
      case "day6" => Day6.run()
      case "day6b" => Day6b.run()
      case "day7" => Day7.run()
      case "day7b" => Day7b.run()
      case "day8" => Day8.run()
      case "day8b" => Day8b.run()
      case "day9" => Day9.run()
      case "day9b" => Day9b.run()
      case "day10" => Day10.run()
      case "day10b" => Day10b.run()
      case _ => println("not found")
    }

  }



}
