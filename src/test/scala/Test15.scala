import org.scalatest._

import challenge.Day15

class Test15 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day15.run() should be (619)
    }
}
