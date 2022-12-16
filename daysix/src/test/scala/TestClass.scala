import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import DaySix._


val testInput = List("mjqjpqmgbljsphdztnvjfqwrcgsmlb", "bvwbjplbgvbhsrlpgdmjqwftvncz", "nppdvjthqldpwncqszvftbrmjlhg", "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "A string with only unique chars" should "be identified" in {
    DaySix.onlyUniqueCharsInString("yyyy".toCharArray) should be (false)
    DaySix.onlyUniqueCharsInString("abcd".toCharArray) should be (true)
    DaySix.onlyUniqueCharsInString("abcb".toCharArray) should be (false)
  }

  "The first marker" should "be found in a datastream buffer" in {
    DaySix.findTheFirstMarker(testInput(0), 4) should be (7)
    DaySix.findTheFirstMarker(testInput(1), 4) should be (5)
    DaySix.findTheFirstMarker(testInput(2), 4) should be (6)
    DaySix.findTheFirstMarker(testInput(3), 4) should be (10)
    DaySix.findTheFirstMarker(testInput(4), 4) should be (11)
  }

  "The start of message marker" should "be found in a datastream buffer" in {
    DaySix.findTheFirstMarker(testInput(0), 14) should be (19)
    DaySix.findTheFirstMarker(testInput(1), 14) should be (23)
    DaySix.findTheFirstMarker(testInput(2), 14) should be (23)
    DaySix.findTheFirstMarker(testInput(3), 14) should be (29)
    DaySix.findTheFirstMarker(testInput(4), 14) should be (26)
  }
}