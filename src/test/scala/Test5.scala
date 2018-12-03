import org.scalatest._

import challenge.Day5

class Test5 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day5.run() should be (374269)
    }
}
