package day7

import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite:
  private val data = Seq(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  )

  test("'part1' should handle all types") {
    val result = part1(
      Seq(
        "AAAAA 1",      // 5 of a kind
        "AA8AA 10",     // 4 of a kind
        "23332 100",    // full house
        "TTT98 1000",   // 3 of a kind
        "23432 10000",  // 2 pair
        "A23A4 100000", // 1 pair
        "23456 1000000" // high card
      )
    )
    assert(result == (1 * 1000000 + 2 * 100000 + 3 * 10000 + 4 * 1000 + 5 * 100 + 6 * 10 + 7 * 1))
  }

  test("'part1' should handle the same type: four of a kind") {
    val result = part1(
      Seq(
        "33332 1",
        "2AAAA 10"
      )
    )
    assert(result == (1 * 10 + 2 * 1))
  }

  test("'part1' should handle the same type: full house") {
    val result = part1(
      Seq(
        "77888 1",
        "77788 10"
      )
    )
    assert(result == (1 * 10 + 2 * 1))
  }

  test("'part1' should handle example") {
    val result = part1(data)
    assert(result == 6440)
  }

  test("'part2' should handle example") {
    val result = part2(data)
    assert(result == 5905)
  }

  test("'part2' should handle all types") {
    val result = part2(
      Seq(
        "AAAAA 1",      // 5 of a kind
        "AA8AA 10",     // 4 of a kind
        "23332 100",    // full house
        "TTT98 1000",   // 3 of a kind
        "23432 10000",  // 2 pair
        "A23A4 100000", // 1 pair
        "23456 1000000" // high card
      )
    )
    assert(result == (1 * 1000000 + 2 * 100000 + 3 * 10000 + 4 * 1000 + 5 * 100 + 6 * 10 + 7 * 1))
  }

  test("'part2' should handle the same type: four of a kind") {
    val result = part2(
      Seq(
        "33332 1",
        "2AAAA 10"
      )
    )
    assert(result == (1 * 10 + 2 * 1))
  }

  test("'part2' should handle the same type: full house") {
    val result = part2(
      Seq(
        "77888 1",
        "77788 10"
      )
    )
    assert(result == (1 * 10 + 2 * 1))
  }

  test("'part2' should handle all types with Joker") {
    val result = part2(
      Seq(
        "AAJAA 1",      // 5 of a kind
        "AJ8JA 10",     // 4 of a kind
        "2J332 100",    // full house
        "TTJ98 1000",   // 3 of a kind
        "23432 10000",  // 2 pair
        "A23J4 100000", // 1 pair
        "23456 1000000" // high card
      )
    )
    assert(result == (1 * 1000000 + 2 * 100000 + 3 * 10000 + 4 * 1000 + 5 * 100 + 6 * 10 + 7 * 1))
  }

  test("'part2' should handle the same type: four of a kind with Joker") {
    val result = part2(
      Seq(
        "23333 1",
        "JAAA2 10"
      )
    )
    assert(result == (1 * 10 + 2 * 1))
  }

  test("'part2' should handle the same type: full house with Joker") {
    val result = part2(
      Seq(
        "7788J 1",
        "77J88 10"
      )
    )
    assert(result == (1 * 10 + 2 * 1))
  }
