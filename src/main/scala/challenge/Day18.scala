package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day18 extends Challenge {

  val registry: mutable.Map[Char, Long] = mutable.Map[Char, Long]('_' -> 0).withDefaultValue(0)

  sealed trait Instruction {
    def exec(): Long

    def isRcv: Boolean = false

    def resolve(arg: String): Long = if (arg.last.isDigit) arg.toLong else registry(arg.last)
  }

  case class Jmp(x: String, y: String) extends Instruction {
    override def exec(): Long = if (resolve(x) > 0) resolve(y) else 1
  }

  case class Snd(x: Char) extends Instruction {
    override def exec(): Long = {
      registry('_') = registry(x)
      1
    }
  }

  case class Set(x: Char, y: String) extends Instruction {
    override def exec(): Long = {
      registry(x) = resolve(y)
      1
    }
  }

  case class Add(x: Char, y: String) extends Instruction {
    override def exec(): Long = {
      registry(x) += resolve(y)
      1
    }
  }

  case class Mul(x: Char, y: String) extends Instruction {
    override def exec(): Long = {
      registry(x) *= resolve(y)
      1
    }
  }

  case class Mod(x: Char, y: String) extends Instruction {
    override def exec(): Long = {
      registry(x) = registry(x) % resolve(y)
      1
    }
  }

  case class Rcv(x: Char) extends Instruction {
    override def isRcv: Boolean = true

    override def exec(): Long = if (registry(x) > 0) 0 else 1
  }

  def parse(cmd: String): Instruction = {
    val parts = cmd.split(' ')
    val args: (String, String) = (parts(1), parts.last)
    parts.head match {
      case "jgz" => Jmp(args._1, args._2)
      case "set" => Set(args._1.head, args._2)
      case "snd" => Snd(args._1.head)
      case "add" => Add(args._1.head, args._2)
      case "mul" => Mul(args._1.head, args._2)
      case "mod" => Mod(args._1.head, args._2)
      case "rcv" => Rcv(args._1.head)
    }
  }

  def exec(instructions: List[Instruction]): Long = {

    @tailrec
    def accumulator(pos: Int): Long = pos match {
      case p if p < instructions.length =>
        val offset: Long = instructions(pos.toInt).exec()
        if (offset == 0 && instructions(pos.toInt).isRcv) registry('_') else accumulator(p + offset.toInt)
      case _ => throw new NoSuchElementException
    }

    accumulator(0)
  }

  override def run(): Any = {
    val input = Source.fromResource("day18.txt").getLines.toList
    exec(input.map(parse))
  }

}
