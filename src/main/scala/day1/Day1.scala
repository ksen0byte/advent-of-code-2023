package day1

@main
def day1(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day1/input.txt"))
  println(part1(input))
  println(part2(input))

def part1(input: Seq[String]): Int =
  input
    .map(str => str.filter(_.isDigit))
    .map(digits => s"${digits.head}${digits.last}")
    .map(_.toInt)
    .sum

case class NumWithIndex(value: Int, index: Int)
case class NumWithStr(value: Int, name: String)
object NumWithStr:
  val digits: Seq[NumWithStr] = Seq(
    NumWithStr(1, "one"),
    NumWithStr(2, "two"),
    NumWithStr(3, "three"),
    NumWithStr(4, "four"),
    NumWithStr(5, "five"),
    NumWithStr(6, "six"),
    NumWithStr(7, "seven"),
    NumWithStr(8, "eight"),
    NumWithStr(9, "nine")
  )

def part2(input: Seq[String]) =
  def findDigits(str: String) =
    str.zipWithIndex
      .collect { case (c, i) if c.isDigit => NumWithIndex(value = c.asDigit, index = i) }
      .filterNot(_.index == -1)

  def findSpelledOutDigits(str: String) =
    NumWithStr.digits
      .flatMap(num =>
        Set(
          NumWithIndex(num.value, index = str.indexOf(num.name)),
          NumWithIndex(num.value, index = str.lastIndexOf(num.name))
        )
      )
      .filterNot(_.index == -1)

  input
    .map(str => findDigits(str) ++ findSpelledOutDigits(str))
    .map { digits =>
      val sortedDigits = digits.sortBy(_.index)
      s"${sortedDigits.head.value}${sortedDigits.last.value}"
    }
    .map(_.toInt)
    .sum
