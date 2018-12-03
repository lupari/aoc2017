import org.scalatest._

import challenge.Day5b

class Test5b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day5b.run() should be (27720699)
    }
}
