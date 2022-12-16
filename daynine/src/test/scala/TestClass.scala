import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

val testInput: List[Tuple2[Char, Int]] = List(('R',4),('U',4),('L',3),('D',1),('R',4),('D',1),('L',5),('R',2))
val testInput2: List[Tuple2[Char, Int]] = List(('R',5),('U',8),('L',8),('D',3),('R',17),('D',10),('L',25),('U',20))

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "A head knot" should "be be able to move" in {
    val head = Knot(1,1)
    testInput.foreach(head.move(_))
    head.xPosition should be (3)
    head.yPosition should be (3)
  }

  "A tail that touches the head" should "be be identified" in {
    val head = Knot(1,1)
    val tail = Knot(1,1)
    val tail2 = Knot(2,3)
    val tail3 = Knot(3,3)
    tail.isTouching(head.xPosition, head.yPosition) should be (true)
    head.move(('R',4))
    tail.isTouching(head.xPosition, head.yPosition) should be (false)
    tail2.isTouching(head.xPosition, head.yPosition) should be (false)
    tail3.isTouching(head.xPosition, head.yPosition) should be (false)
  }

  "A tail" should "follow the head" in {
    val head = Knot(1,1)
    val tail = Knot(1,1)
    head.move(('R',4))
    tail.follow(head)
    tail.xPosition should be (4)
    tail.yPosition should be (1)
    tail.visitedPositions.size should be (4)
    head.move(('U',4))
    tail.follow(head)
    tail.xPosition should be (5)
    tail.yPosition should be (4)
    tail.visitedPositions.size should be (7)
  }

  "All the steps" should "lead to the correct visited positions" in {
    val bridge = new Bridge()
    testInput.foreach(bridge.doStep(_))
    bridge.tail.visitedPositions.size should be (13)
  }

  "A snapped bridge" should "contain 10 knots" in {
    val bridge = new SnappedBridge()
    bridge.allKnots.length should be (10)
  }

  "A snapped bridge" should "move in the right way" in {
    val bridge = new SnappedBridge()
    bridge.doStep(('R',4))
    bridge.allKnots(2).xPosition should be (3)
    bridge.allKnots(2).yPosition should be (1)
    bridge.allKnots(9).xPosition should be (1)
    bridge.allKnots(9).yPosition should be (1)
  }

  "All the steps" should "lead to the correct visited positions for a snapped bridge" in {
    val bridge = new SnappedBridge()
    testInput2.foreach(bridge.doStep(_))
    //System.out.println(bridge.allKnots(9).visitedPositions)
    bridge.allKnots(9).visitedPositions.size should be (36)
  }
}