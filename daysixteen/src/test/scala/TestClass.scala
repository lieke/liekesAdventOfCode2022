import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._


val testInput = List()

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "The supply of a single Elf" should "be calculated correctly" in {
    val singleElfSupply = List(1000, 2000, 3000)
    val totalCaloriesSingleElf = singleElfSupply.sum
    totalCaloriesSingleElf should be (6000)
  }
}