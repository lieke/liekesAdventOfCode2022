import org.scalatest._
import flatspec._
import matchers._

import DayOne.*

val testInput = List(List(1000,2000,3000),List(4000),List(5000,6000),List(7000,8000,9000),List(10000))

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "The supply of a single Elf" should "be calculated correctly" in {
    val singleElfSupply = List(1000, 2000, 3000)
    val totalCaloriesSingleElf = singleElfSupply.sum
    totalCaloriesSingleElf should be (6000)
  }

  "The supplies of multiple Elfs" should "be calculated correctly" in {
    val totalCaloriesAllElfs = DayOne.calculateCaloriesEveryElfCarries(testInput)
    totalCaloriesAllElfs(0) should be (6000)
    totalCaloriesAllElfs(1) should be (4000)
    totalCaloriesAllElfs(3) should be (24000)
  }

  "The max amount of total calories carried by an Elf" should "be calculated correctly" in {
    val maxAmountOfCalories = DayOne.calculateCaloriesEveryElfCarries(testInput).max
    maxAmountOfCalories should be (24000)
  }

  "The sorted list of the calories carried by the elves"  should "be calculated correctly" in {
    val sortedCalorieList = DayOne.sortCalorieListOfElvesFromLowToHigh(DayOne.calculateCaloriesEveryElfCarries(testInput))
    sortedCalorieList should be (List(4000, 6000, 10000, 11000, 24000))
  }

  "The total amount of calories carried by the elves carrying the most" should "be calculated corretly" in {
    val amountOfCaloriesByTopElves = DayOne.getTheTotalAmountOfCaloriesThatTheTopElvesAreCarrying(testInput, 3)
    amountOfCaloriesByTopElves should be (45000)

  }
}