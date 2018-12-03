import org.scalatest._

import challenge.Day18b

class Test18b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day18b.run() should be (5969)
    }
}
