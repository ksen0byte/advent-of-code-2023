package day9

import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite:
  private val example: Seq[String] = Seq("0 3 6 9 12 15", "1 3 6 10 15 21", "10 13 16 21 30 45")

  test("'part1' should handle 1 step extrapolation") {
    val result = part1(Seq(example.head))
    assert(result == 18)
  }

  test("'part1' should handle 2 step extrapolation") {
    val result = part1(Seq(example(1)))
    assert(result == 28)
  }

  test("'part1' should handle 3 step extrapolation") {
    val result = part1(Seq(example(2)))
    assert(result == 68)
  }

  test("'part1' should handle example") {
    val result = part1(example)
    assert(result == 114)
  }

  test("'part2' should handle 1 step extrapolation") {
    val result = part2(Seq(example.head))
    assert(result == -3)
  }

  test("'part2' should handle 2 step extrapolation") {
    val result = part2(Seq(example(1)))
    assert(result == 0)
  }

  test("'part2' should handle 3 step extrapolation") {
    val result = part2(Seq(example(2)))
    assert(result == 5)
  }

  test("'part2' should handle example") {
    val result = part2(example)
    assert(result == 2)
  }
