package day3

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Day3Test extends AnyFunSuite:

  test("'part1' should handle example") {
    val input = Seq(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    )

    val result = part1(input)
    assert(result == 4361)
  }

  test("'part2' should handle example") {
    val input = Seq(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    )

    val result = part2(input)
    assert(result == 467835)
  }
