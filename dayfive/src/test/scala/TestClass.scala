import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._


val testMoves = List((1,2,1), (3,1,3), (2,2,1), (1,1,2))

var testArrangement = new CrateArrangement(List())

class ExampleSpec extends AnyFlatSpec with should.Matchers with BeforeAndAfter {

  before {
    testArrangement = new CrateArrangement(List(new CrateStack('N', 'Z'), new CrateStack('D', 'C', 'M'), new CrateStack('P')))
  }

  "The tops of the test arrangement of crates" should "be given back correctly" in {
    testArrangement.tops() should be ("NDP")
  }

  "A single crate rearrangement procedure" should "be executed correctly" in {
    testArrangement.moveCrate(2,1)
    testArrangement.tops() should be ("DCP")
  }

  "A multiple crate rearrangement procedure" should "be executed corretly" in {
    testArrangement.doMultipleCrateMoves(1,2,1)
    testArrangement.tops() should be ("DCP")
    testArrangement.doMultipleCrateMoves(3,1,3)
    testArrangement.tops() should be ("CZ")
  }

  "All the rearrangement procedures" should " be executed correctly" in {
      testMoves.foreach((moves, from, to) => testArrangement.doMultipleCrateMoves(moves,from,to))
      testArrangement.tops() should be ("CMZ")
  }

  "Multiple crates" should "be able to be picked up at once" in {
    testArrangement.moveMultipleCratesAtOnce(1,2,1)
    testArrangement.tops() should be ("DCP")
    testArrangement.moveMultipleCratesAtOnce(3,1,3)
    testArrangement.tops() should be ("CD")
  }

  "All the rearrangement procedures with multiple crates" should " be executed correctly" in {
    testMoves.foreach((moves, from, to) => testArrangement.moveMultipleCratesAtOnce(moves,from,to))
    testArrangement.tops() should be ("MCD")
  }

}