import org.scalatest._

import challenge.Day20

class Test20 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day20.run() should be (308)
    }
}
