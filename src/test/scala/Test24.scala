import org.scalatest._

import challenge.Day24

class Test24 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day24.run() should be (1695)
    }
}
