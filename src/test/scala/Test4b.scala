import org.scalatest._

import challenge.Day4b

class Test4b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day4b.run() should be (186)
    }
}
