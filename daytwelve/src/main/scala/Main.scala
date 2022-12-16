import scala.util.Random

@main def dayTwelve2022: Unit = {
  val initTime = System.currentTimeMillis
  println("Welcome to day twelve of the Advent of Code!")
  val elapsedTime = System.currentTimeMillis-initTime
  println("This took " + elapsedTime + " milliseconds")
}

val input: List[Int] = List()

class Map {

  var heightRows: List[List[Int]] = _
  var location: (Int, Int) = _
  var bestSignal: (Int, Int) = _
  var currentGeneration: List[Tuple2[Int, List[Tuple2[Int,Int]]]] = _
  val r = new Random()
  val genSize = 100

  def this(input: List[String]) = {
    this()
    heightRows = parseHeightmap(input)
  }

  def getNextGen(previousGen: List[Tuple2[Int,List[Tuple2[Int,Int]]]]): List[Tuple2[Int,List[Tuple2[Int,Int]]]] = {
    var nextGen: List[Tuple2[Int,List[Tuple2[Int,Int]]]] = previousGen.dropRight(previousGen.length/2)
    for (i <- 0 to (previousGen.length / 2)) {
      val newItem: List[Tuple2[Int, Int]] = mutateRandomPath(nextGen(i)._2)
      val newFitness = getFitness(newItem)
      nextGen = (newFitness, newItem) :: nextGen
    }
    nextGen = nextGen.sortWith(_._1 < _._1)
    System.out.println(nextGen(0).toString)
    nextGen
  }

  def getFirstGeneration(): List[Tuple2[Int,List[Tuple2[Int,Int]]]] = {
    var result: List[Tuple2[Int,List[Tuple2[Int,Int]]]] = List()
    for (x <- 0 to genSize) {
      val newPath: List[Tuple2[Int,Int]] = generateRandomPath()
      val fitness = getFitness(newPath)
      result = (fitness, newPath) :: result
    }
    result.sortWith(_._1 < _._1)
  }

  def getFitness(path: List[Tuple2[Int,Int]]): Int = {
    val lastStepInPath = path.head
    (lastStepInPath._1 - bestSignal._1).abs + (lastStepInPath._2 - bestSignal._2).abs
  }

  def mutateRandomPath(oldPath: List[Tuple2[Int,Int]]): List[Tuple2[Int,Int]] = {
    val mutationPoint = r.nextInt(oldPath.length-1)
    var mutatedPath = oldPath.drop(mutationPoint)
    var currentSpot = mutatedPath.head
    while (currentSpot != bestSignal && mutatedPath.length < heightRows.length * heightRows(0).length) {
      val options: List[Tuple2[Int,Int]] = getOptions(currentSpot)
      val filteredOptions: List[Tuple2[Int,Int]] = filterOutPreviousSteps(options,mutatedPath)
      if (filteredOptions.isEmpty)  return mutatedPath
      val newStep = Random.shuffle(filteredOptions).head
      mutatedPath = newStep :: mutatedPath
      currentSpot = newStep
    }
    oldPath
  }

  def generateRandomPath(): List[Tuple2[Int,Int]] = {
    var path: List[Tuple2[Int,Int]] = List()
    var currentSpot: Tuple2[Int, Int] = location
    while (currentSpot != bestSignal && path.length < heightRows.length * heightRows(0).length) {
      val options: List[Tuple2[Int,Int]] = getOptions(currentSpot)
      val filteredOptions: List[Tuple2[Int,Int]] = filterOutPreviousSteps(options,path)
      if (filteredOptions.isEmpty)  return path
      val newStep = Random.shuffle(filteredOptions).head
      path = newStep :: path
      currentSpot = newStep
    }
    path
  }

  def filterOutPreviousSteps(options: List[Tuple2[Int,Int]], existingPath: List[Tuple2[Int,Int]]) : List[Tuple2[Int,Int]] = {
    var filteredOptions: List[Tuple2[Int,Int]] = List()
    for (i <- 0 to options.length -1) {
      val option: Tuple2[Int,Int] = options(i)
      if (!existingPath.contains(option)) filteredOptions = option :: filteredOptions
    }
    filteredOptions
  }

  def getOptions(currentSpot: Tuple2[Int, Int]): List[Tuple2[Int, Int]] = {
    val currentX = currentSpot._1
    val currentY = currentSpot._2
    val currentHeight = heightRows(currentY)(currentX)
    var validOptions: List[Tuple2[Int, Int]] = List()
    if (currentX > 0) {
      val leftOptionHeight = heightRows(currentY)(currentX-1)
      if (currentHeight+2 > leftOptionHeight) validOptions = (currentX-1, currentY) :: validOptions
    }
    if (currentX < heightRows(0).length-2) {
      val rightOptionHeight = heightRows(currentY)(currentX+1)
      if (currentHeight+2 > rightOptionHeight) validOptions = (currentX+1, currentY) :: validOptions
    }
    if (currentY > 0) {
      val topOptionHeight = heightRows(currentY-1)(currentX)
      if (currentHeight+2 > topOptionHeight) validOptions = (currentX, currentY-1) :: validOptions
    }
    if (currentY < heightRows.length-2) {
      val bottomOptionHeight = heightRows(currentY+1)(currentX)
      if (currentHeight+2 > bottomOptionHeight) validOptions = (currentX, currentY+1) :: validOptions
    }
    validOptions
  }

  private def parseHeightmap(input: List[String]): List[List[Int]] = {
    var heightMap: List[List[Int]] = List()
    for (y <- input.length -1 to 0 by -1) {
      val row = input(y).toCharArray().toList
      var heightRow: List[Int] = List()
      for (x <- row.length - 1 to 0 by -1) {
        row(x) match {
          case 'S' => {location = (x, y); heightRow = row(x+1).toInt :: heightRow}
          case 'E' => {bestSignal = (x, y); heightRow = row(x+1).toInt :: heightRow}
          case _ => heightRow = row(x).toInt :: heightRow
        }
      }
      //System.out.println("for row " + y + " heightrow " + heightRow.toString)
      heightMap = heightRow :: heightMap
    }
    heightMap
  }
}