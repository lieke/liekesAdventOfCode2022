@main def dayTen2022: Unit = {
  val initTime = System.currentTimeMillis
  println("Welcome to day ten of the Advent of Code!")
  val xAddValues = 0 :: DayTen.parseInput(input)
  val signalStrengths: List[Int] = DayTen.calculateSignalStrengths(xAddValues)
  val sumOfInterestingSignalStrengths = signalStrengths(19) + signalStrengths(59) + signalStrengths(99) + signalStrengths(139) + signalStrengths(179) + signalStrengths(219)
  println("The sum of the interesting signal strengths is " + sumOfInterestingSignalStrengths)
  val crtRow1 = DayTen.drawCRTRow(0, xAddValues.slice(0,40))
  val crtRow2 = DayTen.drawCRTRow(crtRow1._2, xAddValues.slice(40,80))
  val crtRow3 = DayTen.drawCRTRow(crtRow2._2, xAddValues.slice(80,120))
  val crtRow4 = DayTen.drawCRTRow(crtRow3._2, xAddValues.slice(120,160))
  val crtRow5 = DayTen.drawCRTRow(crtRow4._2, xAddValues.slice(160,200))
  val crtRow6 = DayTen.drawCRTRow(crtRow5._2, xAddValues.slice(200,240))
  println()
  println()
  println(crtRow1)
  println(crtRow2)
  println(crtRow3)
  println(crtRow4)
  println(crtRow5)
  println(crtRow6)
  println()
  println()
  val elapsedTime = System.currentTimeMillis-initTime
  println("This took " + elapsedTime + " milliseconds")
}

object DayTen {

  def drawCRTRow(xValue: Int, xAddValues: List[Int]): (String, Int) = {
    var crtRow:Array[Char] = "........................................".toCharArray
    var spritePosition: List[Int] = List(xValue, xValue+ 1, xValue+2)
    for(i <- 0 to (xAddValues.length-1)) {
      if (spritePosition.contains(i)) crtRow = crtRow.updated(i,'#')
      spritePosition = spritePosition.map(_ + xAddValues(i))
    }
    (crtRow.mkString, spritePosition(0))
  }

  def calculateSignalStrengths(xAddValues: List[Int]): List[Int] = {
    var x: Int = 1
    var signalStrengths: List[Int] = List()
    for (i <- 1 to xAddValues.length-2) {
      x += xAddValues(i-1)
      signalStrengths = signalStrengths :+ (x * i)
    }
    signalStrengths
  }

  def calculateXvalues(xAddValues: List[Int]): List[Int] = {
    var xValue: Int = 1
    var xValues: List[Int] = List()
    for (i <- 1 to xAddValues.length) {
      xValue += xAddValues(i-1)
      xValues = xValues :+ xValue
    }
    xValues
  }

  def parseInput(input: List[String]): List[Int] = {
    input.map(parseInstruction(_)).flatten
  }

  def parseInstruction(instruction: String): List[Int]= {
    instruction match {
      case "noop" => List(0)
      case s"addx $addValue" => List(0,addValue.toInt)
      case _ => null
    }
  }
}

val input: List[String] = List("addx 2","addx 3","addx -2","addx 3","noop","addx 6","addx -1","addx 4","addx 1","noop","addx 3","addx 1","addx 7","noop","noop","addx -1","addx 3","addx 2","noop","addx 4","addx 2","addx -25","addx -7","addx -4","addx 2","addx 2","addx 19","addx -8","addx -5","addx 2","addx -9","addx 16","addx 3","addx -2","addx 12","addx -5","addx 2","addx -15","noop","noop","noop","addx 5","addx 16","addx -22","addx -14","addx 5","noop","addx 29","noop","noop","noop","addx -21","addx 2","noop","noop","addx 5","addx -1","addx 1","noop","noop","addx 8","addx -2","addx 4","noop","addx -22","addx 29","noop","addx -36","noop","addx -2","addx 6","addx -2","addx 2","noop","noop","noop","addx 8","addx 2","addx 10","noop","addx -5","addx 3","addx -2","addx 9","addx -2","addx 2","addx -21","addx 10","addx 17","addx -38","noop","noop","noop","addx 34","addx -27","addx 2","addx -6","addx 7","addx 5","addx 2","addx 5","noop","noop","noop","addx 3","addx -2","addx 2","addx 5","addx 2","addx -29","addx 35","addx -3","addx -25","addx -8","addx 1","noop","addx 4","addx 3","addx -2","addx 5","noop","addx 8","addx -6","noop","addx -3","addx 10","noop","noop","addx 6","addx -1","addx -18","addx 21","addx -30","addx 37","addx 1","noop","noop","noop","noop")