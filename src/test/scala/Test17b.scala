import org.scalatest._

import challenge.Day17b

class Test17b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day17b.run() should be (11162912)
    }
}
