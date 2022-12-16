import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

val testInput: List[String] = List("30373","25512","65332","33549","35390")

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "The horizontal visibility of the trees" should "be determined" in {
    val treeHeights = testInput(0).toCharArray.map(_.asDigit).toList
    DayEight.determineVisibilityForARow(treeHeights) should be (List(true, false, false, true, true))
    val treeHeights2 = testInput(2).toCharArray.map(_.asDigit).toList
    DayEight.determineVisibilityForARow(treeHeights2) should be (List(true, true, false, true, true))
  }

  "The vertical visibility of the trees" should "be determined" in {
    val verticalHeights = testInput.map(_.charAt(0)).map(_.asDigit).toList
    verticalHeights should be (List(3,2,6,3,3))
    DayEight.determineVisibilityForARow(verticalHeights) should be (List(true, false, true, false, true))
  }

  "The horizontal visibility of all the trees" should "be determined" in {
    val allRows = DayEight.getAllAngles(testInput)
    val visibilityMap = allRows.map(DayEight.determineVisibilityForARow(_))
    visibilityMap(0) should be (List(true, false, false, true, true))
    visibilityMap(1) should be (List(true, true, true, false, true))
    visibilityMap(2) should be (List(true, true, false, true, true))
  }

  "The identical trees" should "be filtered from the result" in {
    val horizontalSlices = DayEight.getHorizontalSlices(testInput)
    val verticalSlices = DayEight.getVerticalSlices(testInput)
    DayEight.countTheAmountOfVisibleTrees(horizontalSlices, verticalSlices) should be (21)
  }

  "The scenic score for a single tree" should "be determined" in {
    val horizontalSlice = DayEight.getHorizontalSlices(testInput)(1)
    val horizontalVisibility = DayEight.determineisibilityForDimension(horizontalSlice, 2)
    horizontalVisibility should be (2, 1)
    val verticalSlice = DayEight.getVerticalSlices(testInput)(2)
    val verticalVisibility = DayEight.determineisibilityForDimension(verticalSlice, 1)
    verticalVisibility should be (2, 1)
    val scenicScore = horizontalVisibility._1 * horizontalVisibility._2 * verticalVisibility._1 * verticalVisibility._2
    scenicScore should be (4)
  }

  "The maximum scenic score for each tree" should "be determined" in {
    var maxScenicScore = 0
    val horizontalSlices = DayEight.getHorizontalSlices(testInput)
    val verticalSlices = DayEight.getVerticalSlices(testInput)
    for (xIndex <- 0 to horizontalSlices.length - 1; yIndex <- 0 to verticalSlices.length - 1) {
      val horizontalSlice = horizontalSlices(yIndex)
      val verticalSlice = verticalSlices(xIndex)
      val horizontalVisibility = DayEight.determineisibilityForDimension(horizontalSlice, xIndex)
      val verticalVisibility = DayEight.determineisibilityForDimension(verticalSlice, yIndex)
      val scenicScore = horizontalVisibility._1 * horizontalVisibility._2 * verticalVisibility._1 * verticalVisibility._2
      System.out.println("At index " + xIndex + "," + yIndex + " scenic score is " + maxScenicScore)
      if (scenicScore > maxScenicScore) {
        maxScenicScore = scenicScore
        //System.out.println("At index " + xIndex + "," + yIndex + "scenic score is " + maxScenicScore)
      }
    }
    maxScenicScore should be (8)
  }
}