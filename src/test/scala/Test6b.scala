import org.scalatest._

import challenge.Day6b

class Test6b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day6b.run() should be (1695)
    }
}
