package day6

import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite:
  private val data = Seq(
    "Time:      7  15   30",
    "Distance:  9  40  200"
  )

  test("'part1' should handle example") {
    val result = part1(data)
    assert(result == 288)
  }

  test("'part2' should handle example") {
    val result = part2(data)
    assert(result == 71503)
  }
