import org.scalatest._

import challenge.Day12

class Test12 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day12.run() should be (141)
    }
}
