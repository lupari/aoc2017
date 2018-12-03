import org.scalatest._

import challenge.Day16

class Test16 extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day16.run() should be ("padheomkgjfnblic")
    }
}
