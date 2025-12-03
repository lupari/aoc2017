import org.scalatest._

import challenge.Day11

class Test11 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day11.run() should be (650)
    }
}
