import org.scalatest._

import challenge.Day12b

class Test12b extends FlatSpec with Matchers {

    it should "diplay correct result" in {
        Day12b.run() should be (171)
    }
}
