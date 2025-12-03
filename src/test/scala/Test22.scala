import org.scalatest._

import challenge.Day22

class Test22 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day22.run() should be (5565)
    }
}
