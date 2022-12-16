import collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import org.scalatest._
import flatspec._
import matchers._

import Notes._

val testInput: List[String] = List("Monkey 0:","Starting items: 79, 98","Operation: new = old * 19","Test: divisible by 23","If true: throw to monkey 2","If false: throw to monkey 3","Monkey 1:","Starting items: 54, 65, 75, 74","Operation: new = old + 6","Test: divisible by 19","If true: throw to monkey 2","If false: throw to monkey 0","Monkey 2:","Starting items: 79, 60, 97","Operation: new = old * old","Test: divisible by 13","If true: throw to monkey 1","If false: throw to monkey 3","Monkey 3:","Starting items: 74","Operation: new = old + 3","Test: divisible by 17","If true: throw to monkey 0","If false: throw to monkey 1")

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "All the monkeys" should "should be added" in {
    testInput.foreach(Notes.parseNoteLine(_))
    Notes.monkeyList.length should be (4)
    Notes.monkeyList(2).startingLevels.toList should be (List(79,60,97))
    Notes.monkeyList(3).startingLevels.toList should be (List(74))
    Notes.monkeyList(0).operationString should be ("* 19")
    Notes.monkeyList(0).divisible should be (23)
    Notes.monkeyList(3).divisible should be (17)
    Notes.monkeyList(1).ifTrue should be (2)
    Notes.monkeyList(2).ifFalse should be (3)
  }

  "All the monkeys" should "be be able to do a turn" in {
    Notes.monkeyList.foreach(_.doTurn())
    Notes.monkeyList(0).startingLevels.toList should be (List(20, 23, 27, 26))
    Notes.monkeyList(1).startingLevels.toList should be (List(2080, 25, 167, 207, 401, 1046))
    Notes.monkeyList(2).startingLevels.toList should be (List())
    Notes.monkeyList(3).startingLevels.toList should be (List())
    for (i <- 0 to 18) {
      Notes.monkeyList.foreach(_.doTurn())
    }
    Notes.monkeyList(0).startingLevels.toList should be (List(10, 12, 14, 26, 34))
    Notes.monkeyList(1).startingLevels.toList should be (List(245, 93, 53, 199, 115))
    Notes.monkeyList(2).startingLevels.toList should be (List())
    Notes.monkeyList(3).startingLevels.toList should be (List())

    Notes.monkeyList(0).inspectCount should be (101)
    Notes.monkeyList(1).inspectCount should be (95)
    Notes.monkeyList(2).inspectCount should be (7)
    Notes.monkeyList(3).inspectCount should be (105)
  }

  "All the monkeys" should "be able to do a more worrysome turn" in {
    Notes.monkeyList = List()
    Notes.indexCount = 0
    testInput.foreach(Notes.parseNoteLine(_))
    Notes.monkeyList.foreach(_.doMoreWorryTurn())
    Notes.monkeyList.map(_.inspectCount) should be (List(2, 4, 3, 6))
    for (i <- 0 to 18) {
      Notes.monkeyList.foreach(_.doMoreWorryTurn())
    }
    Notes.monkeyList.map(_.inspectCount) should be (List(99, 97, 8, 103))
    //System.out.println(monkeyBusinessList)
    //val monkeyBusiness: BigInt = BigInt(monkeyBusinessList(0)) * BigInt(monkeyBusinessList(1))
    //System.out.println(monkeyBusiness)
  }
}

