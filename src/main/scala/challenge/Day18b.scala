package challenge

import base.Challenge

import scala.collection.immutable
import scala.io.Source

object Day18b extends Challenge {

  case class State(position: Long, registry: Map[Char, Long], source: immutable.Queue[Long], sink: immutable.Queue[Long], sendCount: Long, isBlocked: Boolean) {
    def inc() = copy(position = position + 1)

    def setRegister(r: Char, value: Long) = copy(registry = registry + (r -> value))

    def getValue(arg: String): Long = if (arg.last.isDigit) arg.toInt else registry.getOrElse(arg.last, 0)

    def getValue(arg: Char): Long = registry.getOrElse(arg, 0)
  }

  object State {
    def empty(id: Long): State = State(0, Map('p' -> id), immutable.Queue.empty, immutable.Queue.empty, 0, isBlocked = true)
  }

  sealed trait Instruction {
    def exec(state: State): State

    def isRcv: Boolean = false

    def isSnd: Boolean = false

  }

  case class Jump(x: String, y: String) extends Instruction {

    override def exec(state: State): State = {
      val inc = if (state.getValue(x) > 0) state.getValue(y) else 1
      state.copy(position = state.position + inc)
    }
  }

  case class Snd(x: Char) extends Instruction {

    override def exec(state: State): State = {
      state.inc().copy(sink = state.sink.enqueue(state.getValue(x)), sendCount = state.sendCount + 1)
    }
  }

  case class Rcv(x: Char) extends Instruction {

    override def exec(state: State): State = {
      if (state.source.isEmpty) state.copy(isBlocked = true)
      else {
        val (m: Long, q: immutable.Queue[Long]) = state.source.dequeue
        state.inc().setRegister(x, m).copy(source = q)
      }
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
    val args: (String, String) = (parts(1), parts.last)
    parts.head match {
      case "jgz" => Jump(args._1, args._2)
      case "set" => Set(args._1.head, args._2)
      case "snd" => Snd(args._1.head)
      case "add" => Add(args._1.head, args._2)
      case "mul" => Mul(args._1.head, args._2)
      case "mod" => Mod(args._1.head, args._2)
      case "rcv" => Rcv(args._1.head)
    }
  }

  override def run(): Any = {
    val instructions: List[Instruction] = Source.fromResource("day18.txt").getLines().map(parse).toList

    def program(program: State): Iterator[State] = runProgram(instructions, program)

    def isBlocked(p: State) = p.isBlocked || p.position < 0 || p.position >= instructions.length

    val run = Iterator.iterate((State.empty(0), State.empty(1))) {
      case (p0, p1) =>
        (
          program(p0.copy(isBlocked = false, sink = immutable.Queue.empty, source = p1.sink)).find(isBlocked).get,
          program(p1.copy(isBlocked = false, sink = immutable.Queue.empty, source = p0.sink)).find(isBlocked).get
        )
    }
    run.drop(1).find {
      case (p0, p1) =>
        p0.sink.isEmpty &&
          p1.sink.isEmpty
    }.get._2.sendCount

  }

  private def runProgram(instructions: Seq[Instruction], state: State): Iterator[State] = {
    Iterator.iterate(state) {
      case s@State(position, _, _, _, _, _) =>
        instructions(position.toInt) match {
          case Snd(r) => Snd(r).exec(s)
          case Set(x, y) => Set(x, y).exec(s)
          case Add(x, y) => Add(x, y).exec(s)
          case Mul(x, y) => Mul(x, y).exec(s)
          case Mod(x, y) => Mod(x, y).exec(s)
          case Jump(x, y) => Jump(x, y).exec(s)
          case Rcv(x) => Rcv(x).exec(s)
        }
    }
  }
}
