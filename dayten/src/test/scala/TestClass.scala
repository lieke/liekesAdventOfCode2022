import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

import DayTen._

val testInput = List("noop","addx 3","addx -5")
val testInput2 = List("addx 15","addx -11","addx 6","addx -3","addx 5","addx -1","addx -8","addx 13","addx 4","noop","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx -35","addx 1","addx 24","addx -19","addx 1","addx 16","addx -11","noop","noop","addx 21","addx -15","noop","noop","addx -3","addx 9","addx 1","addx -3","addx 8","addx 1","addx 5","noop","noop","noop","noop","noop","addx -36","noop","addx 1","addx 7","noop","noop","noop","addx 2","addx 6","noop","noop","noop","noop","noop","addx 1","noop","noop","addx 7","addx 1","noop","addx -13","addx 13","addx 7","noop","addx 1","addx -33","noop","noop","noop","addx 2","noop","noop","noop","addx 8","noop","addx -1","addx 2","addx 1","noop","addx 17","addx -9","addx 1","addx 1","addx -3","addx 11","noop","noop","addx 1","noop","addx 1","noop","noop","addx -13","addx -19","addx 1","addx 3","addx 26","addx -30","addx 12","addx -1","addx 3","addx 1","noop","noop","noop","addx -9","addx 18","addx 1","addx 2","noop","noop","addx 9","noop","noop","noop","addx -1","addx 2","addx -37","addx 1","addx 3","noop","addx 15","addx -21","addx 22","addx -6","addx 1","noop","addx 2","addx 1","noop","addx -10","noop","noop","addx 20","addx 1","addx 2","addx 2","addx -6","addx -11","noop","noop","noop")

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  "The X add values" should "be calculated for the test input" in {
    val xAddValues = DayTen.parseInput(testInput)
    xAddValues should be (List(0,0,3,0,-5))
  }

  "The signal strengths" should "be calculated for each cycle" in {
    val xAddValues = 0 :: DayTen.parseInput(testInput2)
    val signalStrengths: List[Int] = DayTen.calculateSignalStrengths(xAddValues)
    signalStrengths(19) should be (420)
    signalStrengths(59) should be (1140)
    signalStrengths(99) should be (1800)
    signalStrengths(139) should be (2940)
    signalStrengths(179) should be (2880)
    signalStrengths(219) should be (3960)
  }

  "The CRT pixel" should "be drawn at the right position" in {
    val xAddValues2 = DayTen.parseInput(testInput2).slice(0,21)
    val crtRow2 = DayTen.drawCRTRow(0,xAddValues2)
    crtRow2._1.substring(0,21) should be ("##..##..##..##..##..#")
  }

  "All CRT rows" should "be drawn right" in {
    val xAddValues = DayTen.parseInput(testInput2)
    val crtRow1 = DayTen.drawCRTRow(0, xAddValues.slice(0,40))
    println(crtRow1._2 + "  " + xAddValues.slice(0,40).length)
    val crtRow2 = DayTen.drawCRTRow(crtRow1._2, xAddValues.slice(40,80))
    println(crtRow2._2 + "  " + xAddValues.slice(40,80).length)
    val crtRow3 = DayTen.drawCRTRow(crtRow2._2, xAddValues.slice(80,120))
    val crtRow4 = DayTen.drawCRTRow(crtRow3._2, xAddValues.slice(120,160))
    val crtRow5 = DayTen.drawCRTRow(crtRow4._2, xAddValues.slice(160,200))
    val crtRow6 = DayTen.drawCRTRow(crtRow5._2, xAddValues.slice(200,240))
    crtRow1._1 should be ("##..##..##..##..##..##..##..##..##..##..")
    crtRow2._1 should be ("###...###...###...###...###...###...###.")
    crtRow3._1 should be ("####....####....####....####....####....")
    crtRow4._1 should be ("#####.....#####.....#####.....#####.....")
    crtRow5._1 should be ("######......######......######......####")
    crtRow6._1 should be ("#######.......#######.......#######.....")
  }
}