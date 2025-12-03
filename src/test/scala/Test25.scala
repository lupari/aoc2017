import org.scalatest._

import challenge.Day25

class Test25 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day25.run() should be (3732)
    }
}
