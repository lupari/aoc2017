import org.scalatest._

import challenge.Day16b

class Test16b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day16b.run() should be ("bfcdeakhijmlgopn")
    }
}
