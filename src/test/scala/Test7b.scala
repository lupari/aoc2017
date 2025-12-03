import org.scalatest._

import challenge.Day7b

class Test7b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day7b.run() should be (1206)
    }
}
