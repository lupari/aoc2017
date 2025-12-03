import org.scalatest._

import challenge.Day13

class Test13 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day13.run() should be (632)
    }
}
