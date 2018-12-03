import org.scalatest._

import challenge.Day23

class Test23 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day23.run() should be (3969)
    }
}
