import org.scalatest._

import challenge.Day19b

class Test19b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day19b.run() should be (17628)
    }
}
