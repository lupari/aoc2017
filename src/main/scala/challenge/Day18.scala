package challenge

import base.Challenge

import scala.io.Source

object Day18 extends Challenge {

  case class State(position: Long, registry: Map[Char, Long], isBlocked: Boolean) {
    def inc(): State = copy(position = position + 1)

    def setRegister(r: Char, value: Long): State = copy(registry = registry + (r -> value))

    def getValue(arg: String): Long = if (arg.last.isDigit) arg.toInt else registry.getOrElse(arg.last, 0)

    def getValue(arg: Char): Long = registry.getOrElse(arg, 0)
  }

  object State {
    def empty(): State = State(0, Map(), isBlocked = false)
  }

  trait Instruction {
    def exec(state: State): State
  }

  case class Jmp(x: Char, y: String) extends Instruction {
    override def exec(state: State): State = {
      val inc = if (state.getValue(x) > 0) state.getValue(y) else 1
      state.copy(position = state.position + inc)
    }
  }

  case class Snd(x: Char) extends Instruction {
    override def exec(state: State): State = {
      state.inc().setRegister('_', state.getValue(x))
    }
  }

  case class Rcv(x: Char) extends Instruction {
    override def exec(state: State): State = {
      val inc = if (state.getValue(x) > 0) 1 else 0
      if (inc == 1) state.copy(isBlocked = true) else state.inc()
    }
  }

  case class Set(x: Char, y: String) extends Instruction {
    override def exec(state: State): State = {
      state.inc().setRegister(x, state.getValue(y))
    }
  }

  case class Add(x: Char, y: String) extends Instruction {
    override def exec(state: State): State = {
      state.inc().setRegister(x, state.getValue(x) + state.getValue(y))
    }
  }

  case class Mul(x: Char, y: String) extends Instruction {
    override def exec(state: State): State = {
      state.inc().setRegister(x, state.getValue(x) * state.getValue(y))
    }
  }

  case class Mod(x: Char, y: String) extends Instruction {
    override def exec(state: State): State = {
      state.inc().setRegister(x, state.getValue(x) % state.getValue(y))
    }
  }

  def parse(cmd: String): Instruction = {
    val parts = cmd.split(' ')
    val args: (Char, String) = (parts(1).head, parts.last)
    parts.head match {
      case "jgz" => Jmp(args._1, args._2)
      case "set" => Set(args._1, args._2)
      case "snd" => Snd(args._1)
      case "add" => Add(args._1, args._2)
      case "mul" => Mul(args._1, args._2)
      case "mod" => Mod(args._1, args._2)
      case "rcv" => Rcv(args._1)
    }
  }

  private def runProgram(instructions: Seq[Instruction], state: State): State = {
    state match {
      case s@State(pos, _, _) =>
        instructions(pos.toInt) match {
          case Snd(r) => Snd(r).exec(s)
          case Rcv(x) => Rcv(x).exec(s)
          case Set(x, y) => Set(x, y).exec(s)
          case Add(x, y) => Add(x, y).exec(s)
          case Mul(x, y) => Mul(x, y).exec(s)
          case Mod(x, y) => Mod(x, y).exec(s)
          case Jmp(x, y) => Jmp(x, y).exec(s)
        }
    }
  }

  override def run(): Any = {
    val instructions: List[Instruction] = Source.fromResource("day18.txt").getLines().map(parse).toList

    def program(program: State): State = runProgram(instructions, program)

    def isBlocked(p: State) = p.isBlocked || p.position < 0 || p.position >= instructions.length

    val run: Iterator[State] = Iterator.iterate(State.empty())(x => program(x.copy()))

    run.find(isBlocked).get.getValue('_')
  }


}
