val input: List[String] = List("Sensor at x=3907621, y=2895218: closest beacon is at x=3790542, y=2949630","Sensor at x=1701067, y=3075142: closest beacon is at x=2275951, y=3717327","Sensor at x=3532369, y=884718: closest beacon is at x=2733699, y=2000000","Sensor at x=2362427, y=41763: closest beacon is at x=2999439, y=-958188","Sensor at x=398408, y=3688691: closest beacon is at x=2275951, y=3717327","Sensor at x=1727615, y=1744968: closest beacon is at x=2733699, y=2000000","Sensor at x=2778183, y=3611924: closest beacon is at x=2275951, y=3717327","Sensor at x=2452818, y=2533012: closest beacon is at x=2733699, y=2000000","Sensor at x=88162, y=2057063: closest beacon is at x=-109096, y=390805","Sensor at x=2985370, y=2315046: closest beacon is at x=2733699, y=2000000","Sensor at x=2758780, y=3000106: closest beacon is at x=3279264, y=2775610","Sensor at x=3501114, y=3193710: closest beacon is at x=3790542, y=2949630","Sensor at x=313171, y=1016326: closest beacon is at x=-109096, y=390805","Sensor at x=3997998, y=3576392: closest beacon is at x=3691556, y=3980872","Sensor at x=84142, y=102550: closest beacon is at x=-109096, y=390805","Sensor at x=3768533, y=3985372: closest beacon is at x=3691556, y=3980872","Sensor at x=2999744, y=3998031: closest beacon is at x=3691556, y=3980872","Sensor at x=3380504, y=2720962: closest beacon is at x=3279264, y=2775610","Sensor at x=3357940, y=3730208: closest beacon is at x=3691556, y=3980872","Sensor at x=1242851, y=838744: closest beacon is at x=-109096, y=390805","Sensor at x=3991401, y=2367688: closest beacon is at x=3790542, y=2949630","Sensor at x=3292286, y=2624894: closest beacon is at x=3279264, y=2775610","Sensor at x=2194423, y=3990859: closest beacon is at x=2275951, y=3717327")

@main def dayFifteen2022: Unit = {
  val initTime = System.currentTimeMillis
  println("Welcome to day fifteen of the Advent of Code!")
  val beaconMap = new BeaconMap(input, 2000000)
  beaconMap.eleminateBeaconPossibilitiesForTheSensors()
  val notPresent = beaconMap.getTheAmountOfPlacesABeaconCannotBePresent()
  println("The amount of places where a beacon cannot be present in row 2000000 is : " + notPresent)
  val elapsedTime = System.currentTimeMillis-initTime
  println("This took " + elapsedTime + " milliseconds")
}

class BeaconMap() {

  var beacons: List[Tuple2[Int, Int]] = List()
  var sensors: List[Tuple2[Int, Int]] = List()
  var elimMap: Map[Tuple2[Int, Int], Boolean] = Map()
  var maxX: Int = _
  //var maxY: Int = _
  var minX: Int = _
  //var minY: Int = _
  var interestingRow: Int = _

  def this(inputList: List[String], row: Int) = {
    this()
    inputList.map(parseInputLine(_))
    interestingRow = row
    populateElimMap()
  }

  def getTheAmountOfPlacesABeaconCannotBePresent() : Int = {
    val notPresentWithoutTakingBeaconsIntoAccount = elimMap.filter(elem => !elem._2).keys
    notPresentWithoutTakingBeaconsIntoAccount.filter(!beacons.contains(_)).size
  }

  def eleminateBeaconPossibilitiesForTheSensors() = {
    for (i <- 0 to (sensors.length -1)) {
      val sensor = sensors(i)
      val freeDistance = beaconFreeDistanceForSensorNumber(i)
      println("Eliminating the possibilities for sensor " + i + " - " + sensor.toString + " with distance " + freeDistance)
      elimMap = elimMap.map((position, value) => {
        if (distanceBetweenTwoPositions(sensor, position) <= freeDistance) (position -> false) else (position -> value)
      })
      //printElimMap()
    }
  }

  def distanceBetweenTwoPositions(first: Tuple2[Int, Int], second: Tuple2[Int, Int]): Int = {
    val xDist = (first._1 - second._1).abs
    val yDist = (first._2 - second._2).abs
    xDist + yDist
  }

  def beaconFreeDistanceForSensorNumber(number: Int): Int = {
    val sensor = sensors(number)
    val beacon = beacons(number)
    distanceBetweenTwoPositions(sensor, beacon)
  }

  def printElimMap(): Unit = {
    var sb = new StringBuilder("")
    System.out.println(sb.toString)
    sb = new StringBuilder("")
    for (x <- minX to maxX) {
      if (elimMap.apply(x,interestingRow)) sb.append('.') else sb.append('#')
    }
    System.out.println(sb.toString)
  }

  def parseInputLine(line: String) = {
    line match
    case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" => {
      sensors = (sensorX.toInt, sensorY.toInt) :: sensors
      beacons = (beaconX.toInt, beaconY.toInt) :: beacons
    }
    case _ => System.out.println("ERROR ERROR ERROR on input")
  }

  def populateElimMap() = {
    minX = sensors(0)._1
    //minY = sensors(0)._2
    maxX = minX
    //maxY = minY
    for (i <- 0 to sensors.length -1) {
      val sensor = sensors(i)
      val beacon = beacons(i)
      val difference = distanceBetweenTwoPositions(sensor, beacon)
      if (sensor._1 - difference < minX) minX = sensor._1 - difference
      if (sensor._1 + difference > maxX) maxX = sensor._1 + difference
      /*if (sensor._2 - difference < minY) minY = sensor._2 - difference
      if (sensor._2 + difference > maxY) maxY = sensor._2 + difference*/
    }
    System.out.println("Populating map for X " + minX + " to " + maxX)
    for (x <- minX to maxX) {
      elimMap = elimMap + ((x,interestingRow) -> true)
    }
  }
}