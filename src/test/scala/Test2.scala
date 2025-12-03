import org.scalatest._

import challenge.Day2

class Test2 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day2.run() should be (58975)
    }
}
