package challenge

import base.Challenge

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day23 extends Challenge {

  val registry: mutable.Map[Char, Long] = mutable.Map[Char, Long]().withDefaultValue(0)

  sealed trait Instruction {
    def exec(): Long

    def isMul: Boolean = false

    def resolve(arg: String): Long = if (arg.last.isDigit) arg.toLong else registry(arg.last)
  }

  case class Jmp(x: String, y: String) extends Instruction {
    override def exec(): Long = if (resolve(x) != 0) resolve(y) else 1
  }

  case class Set(x: Char, y: String) extends Instruction {
    override def exec(): Long = {
      registry(x) = resolve(y)
      1
    }
  }

  case class Sub(x: Char, y: String) extends Instruction {
    override def exec(): Long = {
      registry(x) -= resolve(y)
      1
    }
  }

  case class Mul(x: Char, y: String) extends Instruction {
    override def isMul: Boolean = true
    override def exec(): Long = {
      registry(x) *= resolve(y)
      1
    }
  }

  def parse(cmd: String): Instruction = {
    val parts = cmd.split(' ')
    val args: (String, String) = (parts(1), parts.last)
    parts.head match {
      case "jnz" => Jmp(args._1, args._2)
      case "set" => Set(args._1.head, args._2)
      case "sub" => Sub(args._1.head, args._2)
      case "mul" => Mul(args._1.head, args._2)
    }
  }

  def exec(instructions: List[Instruction]): Long = {

    @tailrec
    def accumulator(pos: Int, acc: Long): Long = pos match {
      case p if p >= 0 && p < instructions.length =>
        val instruction = instructions(pos.toInt)
        val offset: Long = instruction.exec()
        accumulator(p + offset.toInt, if (instruction.isMul) acc + 1 else acc)
      case _ => acc
    }

    accumulator(0, 0)
  }

  override def run(): Any = {
    val input = Source.fromResource("day23.txt").getLines.toList
    exec(input.map(parse))
  }

}
