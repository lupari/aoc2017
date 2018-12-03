import org.scalatest._

import challenge.Day7

class Test7 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day7.run() should be ("airlri")
    }
}
