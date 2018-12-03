import org.scalatest._

import challenge.Day21

class Test21 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day21.run() should be (133)
    }
}
