import org.scalatest._

import challenge.Day9b

class Test9b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day9b.run() should be (6285)
    }
}
