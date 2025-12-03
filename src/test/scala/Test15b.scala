import org.scalatest._

import challenge.Day15b

class Test15b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day15b.run() should be (290)
    }
}
