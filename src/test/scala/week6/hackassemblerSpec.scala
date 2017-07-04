package week6

import org.scalatest._
import scala.io.Source._

class hackassemblerSpec extends FlatSpec {
  it should "should generate the example file properly" in {
    val testProg = "Prog"
    Main.main(Array(testProg))
    assert(fromFile(s"data/out/$testProg.hack").getLines sameElements fromFile(s"src/test/resources/$testProg.hack").getLines)
  }
}
