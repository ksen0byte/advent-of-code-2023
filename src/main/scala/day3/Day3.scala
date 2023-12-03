package day3

import scala.util.matching.Regex

@main
def day3(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day3/input.txt"))
  println(part1(input))
  println(part2(input))

case class EngineSchematics(numbers: Seq[Num], symbols: Seq[Sym])
case class Num(value: Int, positions: Seq[Position])
case class Sym(value: String, positions: Seq[Position])
case class Position(row: Int, col: Int)
object Position:
  def isAdjacent(l: Position, r: Position): Boolean = math.abs(l.row - r.row) < 2 && math.abs(l.col - r.col) < 2

object EngineSchematics:
  private val numRegex  = "(\\d+)".r
  private val symRegex  = "([^\\d.]+)".r
  private val gearRegex = "(\\*)".r

  def parse(schema: Seq[String]): EngineSchematics =
    val (nums, syms) = schema.zipWithIndex.map { (schemaLine, col) =>
      val numsInLine = numRegex.findAllMatchIn(schemaLine).map { m =>
        Num(value = m.matched.toInt, positions = (m.start until m.end).map(row => Position(row, col)))
      }
      val symbolsInLine = symRegex.findAllMatchIn(schemaLine).map { m =>
        Sym(value = m.matched, positions = (m.start until m.end).map(row => Position(row, col)))
      }
      (numsInLine, symbolsInLine)
    }.unzip
    EngineSchematics(numbers = nums.flatten, symbols = syms.flatten)

  def findPartNums(engineSchematics: EngineSchematics): Seq[Num] =
    val symbolsPositions = engineSchematics.symbols.flatMap(_.positions)
    engineSchematics.numbers.filter { num =>
      num.positions.exists(numPos => symbolsPositions.exists(Position.isAdjacent(numPos, _)))
    }

  def findNumsAdjacentToGears(engineSchematics: EngineSchematics): Seq[Seq[Num]] =
    val gearsPositions = engineSchematics.symbols.filter(_.value == "*").flatMap(_.positions)
    gearsPositions.map { gearPos =>
      engineSchematics.numbers.filter(num => num.positions.exists(Position.isAdjacent(_, gearPos)))
    }
end EngineSchematics

def part1(lines: Seq[String]): Int =
  EngineSchematics
    .findPartNums(EngineSchematics.parse(lines))
    .map(_.value)
    .sum

def part2(lines: Seq[String]) =
  EngineSchematics
    .findNumsAdjacentToGears(EngineSchematics.parse(lines))
    .filter(numsAdjacentToGear => numsAdjacentToGear.length > 1)
    .map(numsAdjacentToGear => numsAdjacentToGear.map(_.value).product)
    .sum
