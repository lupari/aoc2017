import org.scalatest._

import challenge.Day10b

class Test10b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day10b.run() should be ("62e2204d2ca4f4924f6e7a80f1288786")
    }
}
