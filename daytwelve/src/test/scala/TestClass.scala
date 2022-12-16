import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._
import scala.util.Random


val testInput:List[String] = List("Sabqponm","abcryxxl","accszExk","acctuvwj","abdefghi")

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "The height map" should "be be parsed" in {
    val map: Map = new Map(testInput)
    map.location should be((0, 0))
    map.bestSignal should be((5, 2))
    map.heightRows(0) should be(List(97, 97, 98, 113, 112, 111, 110, 109))
    map.heightRows(1) should be(List(97, 98, 99, 114, 121, 120, 120, 108))
    map.heightRows(2) should be(List(97, 99, 99, 115, 122, 120, 120, 107))
    map.heightRows(3) should be(List(97, 99, 99, 116, 117, 118, 119, 106))
    map.heightRows(4) should be(List(97, 98, 100, 101, 102, 103, 104, 105 ))
  }

  "The options for each position" should "be found" in {
    val map: Map = new Map(testInput)
    map.getOptions((0,0)) should be (List((0,1),(1,0)))
    map.getOptions((3,4)) should be (List((4,4),(2,4)))
    map.getOptions((0,1)) should be (List((0,2),(0,0),(1,1)))
    map.getOptions((2,2)) should be (List((2,3), (2,1), (1,2)))
  }

  "A first generation" should "be generated" in {
    val heightMap: Map = new Map(testInput)
    val firstGen: List[Tuple2[Int, List[Tuple2[Int,Int]]]] = heightMap.getFirstGeneration()
    var previousGen = firstGen
    //var nextGen = List()
    for (i <- 0 to 100) {
      previousGen = heightMap.getNextGen(previousGen)
    }
    val secondGen = heightMap.getNextGen(firstGen)
    val thirdGen = heightMap.getNextGen(secondGen)
    val fourthGen = heightMap.getNextGen(thirdGen)
    val fifthGen = heightMap.getNextGen(fourthGen)
    val sixthGen = heightMap.getNextGen(fifthGen)
    val seventhGen = heightMap.getNextGen(sixthGen)


  }
}