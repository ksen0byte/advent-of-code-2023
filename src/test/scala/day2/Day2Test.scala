package day2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class Day2Test extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks:
  private val examples = Table(
    ("game", "number", "part1IsPossible", "part2Product"),
    ("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", 1, true, 48),
    ("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", 2, true, 12),
    ("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", 3, false, 1560),
    ("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", 4, false, 630),
    ("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", 5, true, 36)
  )

  "part1" should "correctly handle all games from example" in {
    val result = part1(examples.map(_._1))
    assert(result == 8)
  }

  forAll(examples) { case (game, number, expectedResult, _) =>
    it should s"correctly handle game [$game] from example" in {
      val result = part1(Seq(game)) != 0
      result shouldEqual expectedResult
    }
  }

  "part2" should "correctly handle all games from example" in {
    val result = part2(examples.map(_._1))
    assert(result == 2286)
  }

  forAll(examples) { case (game, number, _, expectedResult) =>
    it should s"correctly handle game [$game] from example" in {
      val result = part2(Seq(game))
      result shouldEqual expectedResult
    }
  }
