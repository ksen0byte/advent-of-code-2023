package day9

import scala.annotation.tailrec

@main
def day9(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day9/input.txt"))
  println(part1(input))
  println(part2(input))

type History = List[Long]
object History:
  def apply(str: String): History = "(-?\\d+)".r.findAllMatchIn(str).map(_.matched).map(_.toLong).toList

@tailrec
def extrapolate(history: History, acc: Long): Long =
  val diff = history.sliding(2).map(t => t.head - t(1)).toList
  if diff.forall(_ == diff.head) then acc + diff.head
  else extrapolate(diff, acc + diff.head)

def part1(lines: Seq[String]) = lines.map(History(_)).map(_.reverse).map(h => extrapolate(h, h.head)).sum
def part2(lines: Seq[String]) = lines.map(History(_)).map(h => extrapolate(h, h.head)).sum
