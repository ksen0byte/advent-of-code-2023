package day6

@main
def day6(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day6/input.txt"))
  println(part1(input))
  println(part2(input))

case class Race(time: Long, record: Long)
case class Document(races: List[Race])
object Document:
  private def parseNumbers(str: String) = str.split(" ").filter(_.nonEmpty).map(_.strip).map(_.toLong)
  def parse1(str: String): Document =
    val Array(timesStr, recordsStr) = str split "\n"
    val times                       = parseNumbers(timesStr.replace("Time:", ""))
    val records                     = parseNumbers(recordsStr.replace("Distance:", ""))
    val races                       = (times zip records).map { case (time, record) => Race(time, record) }.toList
    Document(races)

  private def parseNumber(str: String) = str.split(" ").filter(_.nonEmpty).map(_.strip).mkString.toLong
  def parse2(str: String): Document =
    val Array(timesStr, recordsStr) = str split "\n"
    val time                        = parseNumber(timesStr.replace("Time:", ""))
    val record                      = parseNumber(recordsStr.replace("Distance:", ""))
    Document(List(Race(time, record)))
end Document

def part1(lines: Seq[String]) =
  val document = Document.parse1(lines.mkString("\n"))
  document.races.map { race =>
    val holdValues = 0L to race.time
    holdValues.map(hold => (race.time - hold) * hold).count(_ > race.record)
  }.product

def part2(lines: Seq[String]) =
  val document   = Document.parse2(lines.mkString("\n"))
  val race       = document.races.head
  val holdValues = 0L to race.time
  holdValues.map(hold => (race.time - hold) * hold).count(_ > race.record)
