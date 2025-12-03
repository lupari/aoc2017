import org.scalatest._

import challenge.Day17

class Test17 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day17.run() should be (1244)
    }
}
