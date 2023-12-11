package day10

import scala.annotation.tailrec
import scala.collection.mutable

@main
def day10(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day10/input.txt"))
  println(part1(input))
  println(part2(input))

/*
| is a vertical pipe connecting north and south.
- is a horizontal pipe connecting east and west.
L is a 90-degree bend connecting north and east.
J is a 90-degree bend connecting north and west.
7 is a 90-degree bend connecting south and west.
F is a 90-degree bend connecting south and east.
 */

enum Side:
  case North, West, East, South
enum Pipe:
  case NS, NE, NW, EW, SW, SE, START, NONE
object Pipe:
  def apply(ch: Char): Pipe = ch match
    case '|' => NS
    case '-' => EW
    case 'L' => NE
    case 'J' => NW
    case '7' => SW
    case 'F' => SE
    case 'S' => START
    case '.' => NONE

  def validConnection(pipe: Pipe, sidePipe: Pipe, connectionSide: Side): Boolean =
    (pipe, sidePipe, connectionSide) match
      case (START | NS | SW | SE, NS | NE | NW | START, Side.South) => true
      case (START | NS | NW | NE, NS | SW | SE | START, Side.North) => true
      case (START | EW | NW | SW, EW | SE | NE | START, Side.West)  => true
      case (START | EW | NE | SE, EW | SW | NW | START, Side.East)  => true
      case _                                                        => false

case class Position(pipe: Pipe, row: Int, col: Int)

class Labyrinth(val positions: Seq[Position]):
  def start: Position = positions.find(_.pipe == Pipe.START).get

  private def manhattanDistance(pos1: Position, pos2: Position): Int =
    (pos1.row - pos2.row).abs + (pos1.col - pos2.col).abs

  private def validConnection(pos1: Position, pos2: Position): Boolean =
    Pipe.validConnection(
      pos1.pipe,
      pos2.pipe,
      connectionSide = (pos1.row.compare(pos2.row), pos1.col.compare(pos2.col)) match
        case (-1, _) => Side.South
        case (1, _)  => Side.North
        case (_, -1) => Side.East
        case (_, _)  => Side.West
    )

  def findNeighbors(position: Position): Seq[Position] =
    positions.filter(manhattanDistance(position, _) == 1).filter(validConnection(position, _))

  def xRay(position: Position): Seq[Seq[Position]] =
    positions.filter(p => p.row < position.row && p.col == position.col)
      :: positions.filter(p => p.row > position.row && p.col == position.col)
      :: positions.filter(p => p.col < position.col && p.row == position.row)
      :: positions.filter(p => p.col > position.col && p.row == position.row)
      :: Nil

object Labyrinth:
  def apply(lines: Seq[String])(shouldIncludeF: Char => Boolean): Labyrinth =
    val positions = for {
      (line, row) <- lines.zipWithIndex
      (ch, col)   <- line.zipWithIndex if shouldIncludeF(ch)
    } yield Position(Pipe(ch), row, col)
    new Labyrinth(positions)

def part1(lines: Seq[String]) =
  val labyrinth = Labyrinth(lines)(_ != '.')

  @tailrec
  def traverse(pos: Position, visited: Set[Position], count: Int): Int =
    val nextOpt = labyrinth.findNeighbors(pos).filterNot(visited.contains).headOption
    nextOpt match
      case Some(next) =>
        println(s"Step #$count: $next")
        traverse(next, visited + pos, count + 1)
      case None =>
        println(s"Step #$count: END")
        count

  traverse(labyrinth.start, Set(), 1) / 2

def part2(lines: Seq[String]) =
  val labyrinth = Labyrinth(lines)(_ => true)
  // if the shape is IN -> casting xray should cross an odd numbers of walls
  labyrinth.positions
    .filter(_.pipe == Pipe.NONE)
    .count { p =>
      val xRays     = labyrinth.xRay(p)
      val wallXRays = xRays.map(_.filterNot(_.pipe == Pipe.NONE))
      wallXRays.forall(_.length % 2 == 1)
    }
