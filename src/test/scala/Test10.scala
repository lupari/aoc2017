import org.scalatest._

import challenge.Day10

class Test10 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day10.run() should be (19591)
    }
}
