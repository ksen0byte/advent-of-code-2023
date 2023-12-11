package day10

import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite:
  test("'part1' should handle simple example") {
    val input = Seq(
      ".....",
      ".S-7.",
      ".|.|.",
      ".L-J.",
      "....."
    )
    val result = part1(input)
    assert(result == 4)
  }

  test("'part1' should handle more complex example") {
    val input = Seq(
      "..F7.",
      ".FJ|.",
      "SJ.L7",
      "|F--J",
      "LJ..."
    )
    val result = part1(input)
    assert(result == 8)
  }

  test("'part2' should handle simple example") {
    val input = Seq(
      "...........",
      ".S-------7.",
      ".|F-----7|.",
      ".||.....||.",
      ".||.....||.",
      ".|L-7.F-J|.",
      ".|..|.|..|.",
      ".L--J.L--J.",
      "..........."
    )
    val result = part2(input)
    assert(result == 4)
  }

  test("'part2' should handle more complex example") {
    val input = Seq(
      ".F----7F7F7F7F-7....",
      ".|F--7||||||||FJ....",
      ".||.FJ||||||||L7....",
      "FJL7L7LJLJ||LJ.L-7..",
      "L--J.L7...LJS7F-7L7.",
      "....F-J..F7FJ|L7L7L7",
      "....L7.F7||L7|.L7L7|",
      ".....|FJLJ|FJ|F7|.LJ",
      "....FJL-7.||.||||...",
      "....L---J.LJ.LJLJ..."
    )
    val result = part2(input)
    assert(result == 8)
  }
