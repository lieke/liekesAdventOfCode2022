import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import DayFour._

val testInput = List(Tuple2(2,4),Tuple2(6,8),Tuple2(2,3),Tuple2(4,5),Tuple2(5,7),Tuple2(7,9),Tuple2(2,8),Tuple2(3,7),Tuple2(6,6),Tuple2(4,6),Tuple2(2,6),Tuple2(4,8))

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "Two assignments" should "fully intersect or not" in {
    val firstCleaningPair = new CleaningPair(Set.range(2,4), Set.range(6,8))
    val secondCleaningPair = new CleaningPair(Set.range(2,8), Set.range(3,7))
    firstCleaningPair.fullyContains() should be (false)
    secondCleaningPair.fullyContains() should be (true)
  }

  "The input" should "be converted into cleaning pairs" in {
    val allClearningPairs = DayFour.convertToCleaningPairs(testInput)
    allClearningPairs(0).fullyContains() should be (false)
    allClearningPairs(1).fullyContains() should be (false)
    allClearningPairs(2).fullyContains() should be (false)
    allClearningPairs(3).fullyContains() should be (true)
    allClearningPairs(4).fullyContains() should be (true)
    allClearningPairs(5).fullyContains() should be (false)
  }

  "The amount of fully intersecting cleaning pairs" should "be found" in {
    val allClearningPairs = DayFour.convertToCleaningPairs(testInput)
    val fullyIntersectingPairs = DayFour.countTheFullyIntersectingCleaningPairs(allClearningPairs)
    fullyIntersectingPairs should be (2)
  }

  "The amount of overlapping clearing pairs" should "be found" in {
    val allClearningPairs = DayFour.convertToCleaningPairs(testInput)
    val overlappingPairs = DayFour.countTheOverlappingCleaningPairs(allClearningPairs)
    overlappingPairs should be (4)


  }
}