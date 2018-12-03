import org.scalatest._

import challenge.Day24b

class Test24b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day24b.run() should be (1673)
    }
}
