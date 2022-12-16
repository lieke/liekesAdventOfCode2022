import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import DayTwo.*

val testInput = List(('A','Y'),('B','X'),('C','Z'))

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "When playing a game of rock paper scissors, the system" should "calculate the correct outcome for each game" in {
    DayTwo.whatIsTheScoreForTheOutcomeOfThisRound(testInput(0)) should be (8)
    DayTwo.whatIsTheScoreForTheOutcomeOfThisRound(testInput(1)) should be (1)
    DayTwo.whatIsTheScoreForTheOutcomeOfThisRound(testInput(2)) should be (6)
  }

  "When playing multiple games of rock paper scissors, the system" should "calculate the total score correctly" in {
    val score = DayTwo.calculateTheTotalScoreForMultipleRoundsOfRockPaperScissors(testInput)
    score should be (15)
  }

  "When playing a game of rock paper scissors, the system" should "calculate the updated outcome for each game correctly" in {
    DayTwo.whatIsTheUpdatedScoreForTheOutcomeOfThisRound(testInput(0)) should be (4)
    DayTwo.whatIsTheUpdatedScoreForTheOutcomeOfThisRound(testInput(1)) should be (1)
    DayTwo.whatIsTheUpdatedScoreForTheOutcomeOfThisRound(testInput(2)) should be (7)
  }

  "When playing multiple games of rock paper scissors, the system" should "calculate the updated total score correctly" in {
    val score = DayTwo.calculateTheUpdatedTotalScoreForMultipleRoundsOfRockPaperScissors(testInput)
    score should be (12)
  }
}