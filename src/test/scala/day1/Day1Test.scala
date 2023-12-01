package day1

import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite:
  test("'part2' should handle only 1 digit") {
    val result = part2(Seq("1"))
    assert(result == 11)
  }

  test("'part2' should handle multiple digits") {
    val result = part2(Seq("123456"))
    assert(result == 16)
  }

  test("'part2' should handle one spelled out digit") {
    val result = part2(Seq("one"))
    assert(result == 11)
  }

  test("'part2' should handle multiple spelled out digits") {
    val result = part2(Seq("onetwothreefourfivesix"))
    assert(result == 16)
  }

  test("'part2' should handle repeated out digits") {
    val result = part2(Seq("onetwoone"))
    assert(result == 11)
  }

  test("'part2' should handle overlapping spelled out digits") {
    assert(part2(Seq("oneight")) == 18)
    assert(part2(Seq("eighthree")) == 83)
    assert(part2(Seq("sevenine")) == 79)
  }

  test("'part2' should handle spelled out and normal digits") {
    val result = part2(Seq("one2"))
    assert(result == 12)
  }

  test("'part2' should handle example") {
    val result = part2(
      Seq(
        "two1nine",
        "eightwothree",
        "abcone2threexyz",
        "xtwone3four",
        "4nineeightseven2",
        "zoneight234",
        "7pqrstsixteen"
      )
    )
    assert(result == 281)
  }
