import org.scalatest._
import flatspec._
import matchers._

import java.lang.StringBuilder


val testInput = List("Sensor at x=2, y=18: closest beacon is at x=-2, y=15","Sensor at x=9, y=16: closest beacon is at x=10, y=16","Sensor at x=13, y=2: closest beacon is at x=15, y=3","Sensor at x=12, y=14: closest beacon is at x=10, y=16","Sensor at x=10, y=20: closest beacon is at x=10, y=16","Sensor at x=14, y=17: closest beacon is at x=10, y=16","Sensor at x=8, y=7: closest beacon is at x=2, y=10","Sensor at x=2, y=0: closest beacon is at x=2, y=10","Sensor at x=0, y=11: closest beacon is at x=2, y=10","Sensor at x=20, y=14: closest beacon is at x=25, y=17","Sensor at x=17, y=20: closest beacon is at x=21, y=22","Sensor at x=16, y=7: closest beacon is at x=15, y=3","Sensor at x=14, y=3: closest beacon is at x=15, y=3","Sensor at x=20, y=1: closest beacon is at x=15, y=3")

class ExampleSpec extends AnyFlatSpec with should.Matchers {

  /*"The beacon and sensor input" should "be parsed" in {
    val beaconMap = new BeaconMap(testInput)
    beaconMap.beacons(0) should be (15,3)
    beaconMap.sensors(0) should be (20,1)
    beaconMap.elimMap.get(-2,0) should be (Some(true))
    beaconMap.elimMap.get(25,22) should be (Some(true))
  }

  "The correct beacon free distance from a sensor" should "be determined" in {
    val beaconMap = new BeaconMap(testInput)
    beaconMap.beaconFreeDistanceForSensorNumber(7) should be (9)
    beaconMap.beaconFreeDistanceForSensorNumber(0) should be (7)
    beaconMap.beaconFreeDistanceForSensorNumber(13) should be (7)
  }*/

  "When the coverage of all the sensors is taken into account the beacon free positions" should "be determined" in {
    val beaconMap = new BeaconMap(testInput, 10)
    beaconMap.eleminateBeaconPossibilitiesForTheSensors()
    val notPresent = beaconMap.getTheAmountOfPlacesABeaconCannotBePresent()
    notPresent should be (26)
  }
}

