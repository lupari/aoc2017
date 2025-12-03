import org.scalatest._

import challenge.Day18

class Test18 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day18.run() should be (1187)
    }
}
