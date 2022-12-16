import scala.collection.mutable.ListBuffer

val input: List[String] = List("Monkey 0:","Starting items: 52, 60, 85, 69, 75, 75","Operation: new = old * 17","Test: divisible by 13","If true: throw to monkey 6","If false: throw to monkey 7","Monkey 1:","Starting items: 96, 82, 61, 99, 82, 84, 85","Operation: new = old + 8","Test: divisible by 7","If true: throw to monkey 0","If false: throw to monkey 7","Monkey 2:","Starting items: 95, 79","Operation: new = old + 6","Test: divisible by 19","If true: throw to monkey 5","If false: throw to monkey 3","Monkey 3:","Starting items: 88, 50, 82, 65, 77","Operation: new = old * 19","Test: divisible by 2","If true: throw to monkey 4","If false: throw to monkey 1","Monkey 4:","Starting items: 66, 90, 59, 90, 87, 63, 53, 88","Operation: new = old + 7","Test: divisible by 5","If true: throw to monkey 1","If false: throw to monkey 0","Monkey 5:","Starting items: 92, 75, 62","Operation: new = old * old","Test: divisible by 3","If true: throw to monkey 3","If false: throw to monkey 4","Monkey 6:","Starting items: 94, 86, 76, 67","Operation: new = old + 1","Test: divisible by 11","If true: throw to monkey 5","If false: throw to monkey 2","Monkey 7:","Starting items: 57","Operation: new = old + 2","Test: divisible by 17","If true: throw to monkey 6","If false: throw to monkey 2")

@main def dayEleven2022: Unit = {
  val initTime = System.currentTimeMillis
  println("Welcome to day eleven of the Advent of Code!")
  input.foreach(Notes.parseNoteLine(_))
  System.out.println(Notes.monkeyList.length)
  for (i <- 0 to 19) {
    Notes.monkeyList.foreach(_.doTurn())
  }
  val monkeyBusinessList: List[Long] = Notes.monkeyList.map(_.inspectCount).sortWith(_ > _)
  val monkeyBusiness = monkeyBusinessList(0) * monkeyBusinessList(1)
  System.out.println("The level of monkey business after 20 rounds is: " + monkeyBusiness)

  Notes.monkeyList = List()
  Notes.indexCount = 0
  input.foreach(Notes.parseNoteLine(_))
  for (i <- 0 to 9999) {
    println("At round : " + i)
    Notes.monkeyList.foreach(_.doMoreWorryTurn())
  }
  val moreWorryMonkeyBusinessList: List[Long] = Notes.monkeyList.map(_.inspectCount).sortWith(_ > _)
  println(moreWorryMonkeyBusinessList)
  val moreWorryMonkeyBusiness = moreWorryMonkeyBusinessList(0) * moreWorryMonkeyBusinessList(1)
  System.out.println("The level of monkey business after 1000 rounds is: " + moreWorryMonkeyBusiness)

  val elapsedTime = System.currentTimeMillis-initTime
  println("This took " + elapsedTime + " milliseconds")
}

object Notes {
  var indexCount: Int = 0
  var monkeyList: List[Monkey] = List()


  def parseNoteLine(inputLine: String) = {
    inputLine match {
      case s"Monkey $monkeyNumber:" => monkeyList = monkeyList :+ new Monkey();
      case s"Starting items: $items" => val splitItems = items.split(", ").map(_.toLong); monkeyList(indexCount).startingLevels.appendAll(splitItems.toList)
      case s"Operation: new = old $operationString" => monkeyList(indexCount).operationString = operationString
      case s"Test: divisible by $divisible" => monkeyList(indexCount).divisible = divisible.toInt
      case s"If true: throw to monkey $trueNumber" => monkeyList(indexCount).ifTrue = trueNumber.toInt
      case s"If false: throw to monkey $falseNumber" => monkeyList(indexCount).ifFalse = falseNumber.toInt; indexCount += 1
      case _ =>
    }
  }
}

class Monkey() {
  var startingLevels: ListBuffer[Long] = ListBuffer()
  var operationString: String = ""
  var divisible: Int = 0
  var ifTrue: Int = 0
  var ifFalse: Int = 0
  var inspectCount: Long = 0

  def doTurn(): Unit = {
    startingLevels = startingLevels.map(doOperation(_)).map(_ / 3)
    inspectCount += startingLevels.length
    startingLevels.foreach(item => {
      if (item % divisible == 0) Notes.monkeyList(ifTrue).addItem(item) else Notes.monkeyList(ifFalse).addItem(item)
    })
    startingLevels = new ListBuffer[Long]()
  }

  def doMoreWorryTurn(): Unit = {
    System.out.println(startingLevels.toString())
    startingLevels = startingLevels.map(doOperation(_))
    inspectCount += startingLevels.length
    startingLevels.foreach(item => {
      if (item % divisible == 0) Notes.monkeyList(ifTrue).addItem(item) else Notes.monkeyList(ifFalse).addItem(item)
    })
    startingLevels = new ListBuffer[Long]()
  }

  def addItem(newItem: Long) = {
    startingLevels = startingLevels :+ newItem

  }

  def doOperation(oldValue: Long) = {
    operationString match {
      case "* old" => oldValue * oldValue
      case "+ old" => oldValue + oldValue
      case s"* $timesNumber" => oldValue * timesNumber.toLong
      case s"+ $addNumber" => oldValue + addNumber.toLong
    }
  }
}

