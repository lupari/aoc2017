import org.scalatest._

import challenge.Day1b

class Test1b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day1b.run() should be (1508)
    }
}