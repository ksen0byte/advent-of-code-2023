package day4

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Day4Test extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:
  private val examples = Table(
    ("card", "part1Result"),
    ("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", 8),
    ("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", 2),
    ("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", 2),
    ("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83", 1),
    ("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36", 0),
    ("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11", 0)
  )

  "part1" should "correctly handle all games from example" in {
    val result = part1(examples.map(_._1))
    assert(result == 13)
  }

  forAll(examples) { case (card, expectedResult) =>
    it should s"correctly handle card [$card] from example" in {
      part1(Seq(card)) shouldEqual expectedResult
    }
  }

  "part2" should "correctly handle all games from example" in {
    val result = part2(examples.map(_._1))
    assert(result == 30)
  }
