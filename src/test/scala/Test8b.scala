import org.scalatest._

import challenge.Day8b

class Test8b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day8b.run() should be (8997)
    }
}
