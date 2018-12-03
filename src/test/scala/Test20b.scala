import org.scalatest._

import challenge.Day20b

class Test20b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day20b.run() should be (504)
    }
}
