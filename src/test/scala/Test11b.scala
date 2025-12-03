import org.scalatest._

import challenge.Day11b

class Test11b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day11b.run() should be (1465)
    }
}
