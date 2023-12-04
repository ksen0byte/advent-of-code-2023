package day4

import scala.util.matching.Regex

@main
def day4(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day4/input.txt"))
  println(part1(input))
  println(part2(input))

case class Card(number: Int, winning: Seq[Int], current: Seq[Int])
case class MatchingCard(card: Card, total: Int)
object Card:
  private val cardRegex = "Card +(\\d+): (.+?) \\| (.+)".r
  private def parseNumbers(str: String) =
    str.split(" ").filterNot(_.isBlank).map(_.trim.toInt)

  def parse(str: String): Card = str match
    case cardRegex(num, winningStr, currentStr) => Card(num.toInt, parseNumbers(winningStr), parseNumbers(currentStr))
    case _                                      => throw new IllegalArgumentException(s"Failed to parse [$str] to Card")

  def matchNumbers(card: Card): MatchingCard =
    MatchingCard(card, total = card.winning.intersect(card.current).length)
end Card

def part1(lines: Seq[String]) = lines
  .map(Card.parse)
  .map(Card.matchNumbers)
  .collect { case matchingCard if matchingCard.total > 0 => math.pow(2, matchingCard.total - 1).toInt }
  .sum

case class CardCopy(number: Int, total: Int, instances: Int)
def part2(lines: Seq[String]) =
  val cards = lines.map(Card.parse).map(Card.matchNumbers)
  cards
    .foldLeft(Seq.empty[MatchingCard]) { case (acc, matchingCard) =>
      val nextCardNum           = matchingCard.card.number
      val numberOfInstances     = acc.count(mc => mc.card.number == nextCardNum) + 1
      val copiesFromOneInstance = cards.slice(nextCardNum, nextCardNum + matchingCard.total)
      val totalCopies           = Seq.fill(numberOfInstances)(copiesFromOneInstance).flatten
      acc ++ totalCopies :+ matchingCard
    }
    .length
