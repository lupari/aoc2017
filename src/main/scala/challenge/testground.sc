

val cmd = "b inc 5 if a >= 1"

/*case class Inc(reg: Char, n: Int, cond: (Char, String, Int)) {
  def this(cmd: String) {
    this(cmd.last)
  }

}*/

cmd.head
cmd.dropWhile(c => !c.isDigit).takeWhile(c => c.isDigit).toInt
val condPart = cmd.split(" if ").last.split(" ")
val cond = (condPart(0), condPart(1), condPart(2).toInt)
