import org.scalatest._

import challenge.Day4

class Test4 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day4.run() should be (455)
    }
}
