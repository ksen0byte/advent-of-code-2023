package day7

@main
def day7(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day7/input.txt"))
  println(part1(input))
  println(part2(input))

case class Hand(cards: List[Card], bid: Int, handType: HandType)
enum HandType:
  case HighestCard, OnePair, TwoPairs, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
enum Card:
  case Joker, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace

object HandType:
  def parse(cards: List[Card])(using jCard: Card): HandType =
    val frequencyMap = cards.groupBy(identity).view.mapValues(_.size)
    val jokerFrequency = jCard match
      case Card.Jack  => 0
      case Card.Joker => frequencyMap.getOrElse(jCard, 0)
      case _          => throw new IllegalArgumentException(s"'J' could only be Joker or Jack, got $jCard")

    val maxFrequency = frequencyMap.filterNot(_._1 == Card.Joker).values.maxOption.getOrElse(0)

    frequencyMap.values match
      case frequency if jokerFrequency + maxFrequency == 5                  => HandType.FiveOfAKind
      case frequency if jokerFrequency + maxFrequency == 4                  => HandType.FourOfAKind
      case frequency if maxFrequency == 3 && frequency.exists(_ == 2)       => HandType.FullHouse
      case frequency if frequency.count(_ == 2) == 2 && jokerFrequency == 1 => HandType.FullHouse
      case frequency if jokerFrequency + maxFrequency == 3                  => HandType.ThreeOfAKind
      case frequency if frequency.count(_ == 2) == 2                        => HandType.TwoPairs
      case frequency if jokerFrequency + maxFrequency == 2                  => HandType.OnePair
      case _                                                                => HandType.HighestCard
end HandType

object Card:
  private val numericOrdinalOffset = 1
  def parse(ch: Char)(using Card): Card = ch match
    case 'A'                                              => Card.Ace
    case 'K'                                              => Card.King
    case 'Q'                                              => Card.Queen
    case 'J'                                              => summon[Card]
    case 'T'                                              => Card.Ten
    case numerical if (2 to 9) contains numerical.asDigit => Card.fromOrdinal(numerical.asDigit - numericOrdinalOffset)
    case _ => throw new IllegalArgumentException(s"Expected one of [A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2], got $ch")
end Card

object Hand:
  def parse(str: String)(using Card): Hand =
    val Array(cardsStr, bidStr) = str.trim.split(" ")
    val cards                   = cardsStr.map(Card.parse).toList
    Hand(cards, bid = bidStr.toInt, handType = HandType.parse(cards))

  given Ordering[Hand] with
    override def compare(x: Hand, y: Hand): Int =
      if x.handType.ordinal == y.handType.ordinal then // compare first non-identical pair of cards
        x.cards.zip(y.cards).collectFirst { case (xc, yc) if xc != yc => xc.ordinal.compare(yc.ordinal) }.getOrElse(0)
      else x.handType.ordinal compare y.handType.ordinal
end Hand

def solve(lines: Seq[String])(using Card) =
  lines.map(Hand.parse).sorted.zipWithIndex.map { case (hand, rank) => hand.bid * (rank + 1) }.sum

def part1(lines: Seq[String]) = solve(lines)(using Card.Jack)

def part2(lines: Seq[String]) = solve(lines)(using Card.Joker)
