import org.scalatest._

import challenge.Day9

class Test9 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day9.run() should be (11846)
    }
}
