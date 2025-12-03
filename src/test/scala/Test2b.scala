import org.scalatest._

import challenge.Day2b

class Test2b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day2b.run() should be (308)
    }
}
