import org.scalatest._

import challenge.Day21b

class Test21b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day21b.run() should be (2221990)
    }
}
