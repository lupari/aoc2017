import org.scalatest._

import challenge.Day13b

class Test13b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day13b.run() should be (3849742)
    }
}
