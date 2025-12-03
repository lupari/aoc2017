package challenge

import base.Challenge

import scala.collection.mutable
import scala.io.Source

object Day18b extends Challenge {

  sealed abstract class Instruction {
    def exec(registry: mutable.Map[Char, Long]): Long

    def isRcv: Boolean = false
    def isSnd: Boolean = false

    def resolve(arg: String, registry: mutable.Map[Char, Long]): Long = 
      if (arg.last.isDigit) arg.toLong else registry(arg.last)
  }

  case class Jump(x: String, y: String) extends Instruction {
    override def exec(registry: mutable.Map[Char, Long]): Long = {
      if (resolve(x, registry) > 0) resolve(y, registry) else 1
    }
  }

  case class Snd(x: Char, queue: mutable.Queue[Long]) extends Instruction {
    override def isSnd: Boolean = true
    override def exec(registry: mutable.Map[Char, Long]): Long = {
      queue.enqueue(registry(x))
      1
    }
  }

  case class Set(x: Char, y: String) extends Instruction {
    override def exec(registry: mutable.Map[Char, Long]): Long = {
      registry(x) = resolve(y, registry)
      1
    }
  }

  case class Add(x: Char, y: String) extends Instruction {
    override def exec(registry: mutable.Map[Char, Long]): Long = {
      registry(x) += resolve(y, registry)
      1
    }
  }

  case class Mul(x: Char, y: String) extends Instruction {
    override def exec(registry: mutable.Map[Char, Long]): Long = {
      registry(x) *= resolve(y, registry)
      1
    }
  }

  case class Mod(x: Char, y: String) extends Instruction {
    override def exec(registry: mutable.Map[Char, Long]): Long = {
      registry(x) = registry(x) % resolve(y, registry)
      1
    }
  }

  case class Rcv(x: Char, queue: mutable.Queue[Long]) extends Instruction {

    override def isRcv: Boolean = true

    override def exec(registry: mutable.Map[Char, Long]): Long = {
      if (queue.nonEmpty) {
        registry(x) = queue.dequeue()
        1
      } else {
        0
      }
    }
  }

  case class Program(id: Int, source: mutable.Queue[Long], sink: mutable.Queue[Long], 
    input: List[String], var pointer: Long = 0) {
    
    val registry: mutable.Map[Char, Long] = mutable.Map[Char, Long]('p' -> id.toLong).withDefaultValue(0)
    var sendCount: Int = 0
    val instructions: List[Instruction] = input.map(parse)

    def parse(cmd: String): Instruction = {
      val parts = cmd.split(' ')
      val args: (String, String) = (parts(1), parts.last)
      parts.head match {
        case "jgz" => Jump(args._1, args._2)
        case "set" => Set(args._1.head, args._2)
        case "snd" => Snd(args._1.head, sink)
        case "add" => Add(args._1.head, args._2)
        case "mul" => Mul(args._1.head, args._2)
        case "mod" => Mod(args._1.head, args._2)
        case "rcv" => Rcv(args._1.head, source)
      }
    }


    def isRunning: Boolean = !instructions(pointer.toInt).isRcv || source.nonEmpty

    def run(): Unit = {
      val instruction = instructions(pointer.toInt)
      val offset: Long = instruction.exec(registry)
      if (instruction.isSnd) sendCount = sendCount + 1
      pointer += offset
    }

  }

  override def run(): Any = {
    val input = Source.fromResource("day18.txt").getLines.toList
    val queue1: mutable.Queue[Long] = mutable.Queue.empty
    val queue2: mutable.Queue[Long] = mutable.Queue.empty
    val prog1: Program = Program(0, source=queue1, sink=queue2, input)
    val prog2: Program = Program(1, source=queue2, sink=queue1, input)

    while (prog1.isRunning || prog2.isRunning) {
      while (prog1.isRunning) {
        prog1.run()
      }
      while (prog2.isRunning) {
        prog2.run()
      }
    }
    prog2.sendCount

  }

}
