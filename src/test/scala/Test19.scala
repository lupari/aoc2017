import org.scalatest._

import challenge.Day19

class Test19 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day19.run() should be ("GEPYAWTMLK")
    }
}
