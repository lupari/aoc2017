import org.scalatest._

import challenge.Day14

class Test14 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day14.run() should be (8316)
    }
}
