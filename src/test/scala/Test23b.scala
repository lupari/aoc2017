import org.scalatest._

import challenge.Day23b

class Test23b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day23b.run() should be (917)
    }
}
