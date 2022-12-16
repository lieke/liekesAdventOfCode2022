import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import DayThree.*

val testInput = List("vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn","ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw")

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "Rucksack of an Elf" should "be split in two halfway" in {
    val singleRucksack = testInput(0)
    val rucksackHalves = DayThree.splitRucksackInHalves(singleRucksack)
    rucksackHalves(0) should be ("vJrwpWtwJgWr")
    rucksackHalves(1) should be ("hcsFMMfFFhFp")
  }

  "An element that is in the other rucksack half" should "be found" in {
    val rucksackHalves = DayThree.splitRucksackInHalves(testInput(0))
    val firstElement = rucksackHalves._1(0)
    val notInOtherRucksack = rucksackHalves._2.contains(firstElement)
    notInOtherRucksack should be (false)
    val fifthElement = rucksackHalves._1(4)
    val inOtherRucksack = rucksackHalves._2.contains(fifthElement)
    inOtherRucksack should be (true)
  }

  "The element that is in two rucksack halves" should "be found" in {
    val duplicates1 = DayThree.findTheOnlyElementThatIsInBothHalvesOfARucksack(testInput(0))
    val duplicates2 = DayThree.findTheOnlyElementThatIsInBothHalvesOfARucksack(testInput(1))
    duplicates1 should be ('p')
    duplicates2 should be ('L')
  }

  "The score of the duplicate element" should "be properly calculated" in {
    DayThree.calculateCharValue('p') should be (16)
    DayThree.calculateCharValue('L') should be (38)
    DayThree.calculateCharValue('P') should be (42)
    DayThree.calculateCharValue('v') should be (22)
    DayThree.calculateCharValue('t') should be (20)
    DayThree.calculateCharValue('s') should be (19)
  }

  "The score of the duplicate elements of the entire rucksack list" should "be properly calculated" in {
    val duplicateList = testInput.map(DayThree.findTheOnlyElementThatIsInBothHalvesOfARucksack(_))
    val scoreList = duplicateList.map(DayThree.calculateCharValue(_))
    duplicateList should be (List('p', 'L', 'P', 'v', 't', 's'))
    scoreList should be (List(16, 38, 42, 22, 20, 19))
    val totalScore = DayThree.calculateTotalScoreOfDuplicateValues(testInput)
    totalScore should be (157)
  }

  "The rucksacks" should "be divided in groups of three" in {
    val elfGroups = DayThree.splitIntoElfGroups(testInput)
    elfGroups(0) should be (List("vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"))
  }

  "The duplicate element of a rucksack group" should "be found" in {
    val firstDuplicateElement = DayThree.findTheOnlyElementThatIsInTheThreeRucksacks(DayThree.splitIntoElfGroups(testInput)(0))
    val secondDuplicateElement = DayThree.findTheOnlyElementThatIsInTheThreeRucksacks(DayThree.splitIntoElfGroups(testInput)(1))
    firstDuplicateElement should be ('r')
    secondDuplicateElement should be ('Z')
  }

  "The total scores of the badges" should "be properly calculated" in {
    val badgeValues = DayThree.calculateBadgeValues(testInput)
    badgeValues should be (70)
  }
}