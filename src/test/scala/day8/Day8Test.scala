package day8

import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends AnyFunSuite:
  test("'part1' should handle example with full path") {
    val result = part1(
      Seq(
        "RL",
        "",
        "AAA = (BBB, CCC)",
        "BBB = (DDD, EEE)",
        "CCC = (ZZZ, GGG)",
        "DDD = (DDD, DDD)",
        "EEE = (EEE, EEE)",
        "GGG = (GGG, GGG)",
        "ZZZ = (ZZZ, ZZZ)"
      )
    )
    assert(result == 2)
  }

  test("'part1' should handle example with repeated path") {
    val result = part1(
      Seq(
        "LLR",
        "",
        "AAA = (BBB, BBB)",
        "BBB = (AAA, ZZZ)",
        "ZZZ = (ZZZ, ZZZ)"
      )
    )
    assert(result == 6)
  }

  test("'part2' should handle example") {
    val result = part2(
      Seq(
        "LR",
        "",
        "11A = (11B, XXX)",
        "11B = (XXX, 11Z)",
        "11Z = (11B, XXX)",
        "22A = (22B, XXX)",
        "22B = (22C, 22C)",
        "22C = (22Z, 22Z)",
        "22Z = (22B, 22B)",
        "XXX = (XXX, XXX)"
      )
    )
    assert(result == 6)
  }
