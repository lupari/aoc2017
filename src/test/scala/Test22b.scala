import org.scalatest._

import challenge.Day22b

class Test22b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day22b.run() should be (2511978)
    }
}
