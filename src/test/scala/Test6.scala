import org.scalatest._

import challenge.Day6

class Test6 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day6.run() should be (7864)
    }
}
