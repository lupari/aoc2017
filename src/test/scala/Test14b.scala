import org.scalatest._

import challenge.Day14b

class Test14b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day14b.run() should be (1074)
    }
}
