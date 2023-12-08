package day8

import scala.annotation.tailrec

@main
def day8(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day8/input.txt"))
  println(part1(input))
  println(part2(input))

type NodeKey = String
enum Direction:
  case Left, Right
object Directions:
  def apply(str: String): LazyList[Direction] = LazyList.continually(str.to(LazyList)).flatten.map {
    case 'L' => Direction.Left
    case 'R' => Direction.Right
  }

case class Node(key: NodeKey, left: NodeKey, right: NodeKey)
object Node:
  private val nodeRegex = "(\\w+) = \\((\\w+), (\\w+)\\)".r
  def apply(str: String): Node = str match
    case nodeRegex(key, leftKey, rightKey) => new Node(key, leftKey, rightKey)

  def parse(nodes: Seq[String]): Map[NodeKey, Node] =
    nodes.map(Node(_)).groupBy(_.key).mapValues(_.head).toMap

def part1(lines: Seq[String]) =
  val nodeMap    = Node.parse(lines.drop(2))
  val directions = Directions(lines.head)
  val start      = "AAA"
  val finish     = "ZZZ"

  @tailrec
  def follow(node: NodeKey, directions: LazyList[Direction], count: Int): Int =
    if node == finish then count
    else
      val next = directions.head match
        case Direction.Left  => nodeMap(node).left
        case Direction.Right => nodeMap(node).right
      follow(next, directions.tail, count + 1)

  follow(start, directions, 0)

def part2(lines: Seq[String]) =
  val nodeMap         = Node.parse(lines.drop(2))
  val directions      = Directions(lines.head)
  val startNodes      = nodeMap.collect { case (key, node) if key.endsWith("A") => key }.toList
  val finishCondition = (node: NodeKey) => node.endsWith("Z")

  def lcm(a: BigInt, b: BigInt) = (a * b).abs / (a gcd b)

  @tailrec
  def follow(node: NodeKey, directions: LazyList[Direction], count: Int): Int =
    if finishCondition(node) then count
    else
      val next = directions.head match
        case Direction.Left  => nodeMap(node).left
        case Direction.Right => nodeMap(node).right
      follow(next, directions.tail, count + 1)

  startNodes.map(follow(_, directions, 0)).map(BigInt(_)).foldLeft(BigInt(1))(lcm)
