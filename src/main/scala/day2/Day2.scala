package day2

import scala.util.matching.Regex

@main
def day2(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day2/input.txt"))
  println(part1(input))
  println(part2(input))

case class Game(id: Int, rounds: List[Round])
case class Round(cubes: List[Cube])
case class Cube(color: Color, num: Int)
enum Color:
  case Red, Green, Blue
object Game:
  private val gameRegex: Regex  = "Game (\\d+): (.+)".r
  private val redRegex: Regex   = "(\\d+) red".r
  private val greenRegex: Regex = "(\\d+) green".r
  private val blueRegex: Regex  = "(\\d+) blue".r

  def parse(str: String): Game = str match
    case gameRegex(gameId, roundsStr) =>
      val rounds = roundsStr.split(";").map(_.trim).map { round =>
        val cubes = round
          .split(",")
          .map(_.trim)
          .map {
            case redRegex(num)   => Cube(Color.Red, num.toInt)
            case greenRegex(num) => Cube(Color.Green, num.toInt)
            case blueRegex(num)  => Cube(Color.Blue, num.toInt)
            case unknownCube     => throw new IllegalArgumentException(s"Can't parse cube [$unknownCube] to Cube")
          }
        Round(cubes.toList)
      }
      Game(id = gameId.toInt, rounds.toList)
    case _ => throw new IllegalArgumentException(s"Can't parse string [$str] to Game")

  def possible(game: Game): Boolean =
    val constraints = List(Cube(Color.Red, 12), Cube(Color.Green, 13), Cube(Color.Blue, 14))
    !game.rounds.flatMap(_.cubes).exists { cube =>
      constraints.exists(constraintCube => cube.color == constraintCube.color && cube.num > constraintCube.num)
    }

  def minimumSet(game: Game): List[Cube] =
    game.rounds
      .flatMap(_.cubes)
      .groupBy(_.color)
      .values
      .map(_.maxBy(_.num))
      .toList

end Game

def part1(lines: Seq[String]) = lines
  .map(Game.parse)
  .filter(Game.possible)
  .map(_.id)
  .sum

def part2(lines: Seq[String]) = lines
  .map(Game.parse)
  .map(game => Game.minimumSet(game).map(_.num).product)
  .sum
