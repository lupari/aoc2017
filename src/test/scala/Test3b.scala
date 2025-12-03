import org.scalatest._

import challenge.Day3b

class Test3b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day3b.run() should be (349975)
    }
}
