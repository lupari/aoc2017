import org.scalatest._

import challenge.Day3

class Test3 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day3.run() should be (480)
    }
}
