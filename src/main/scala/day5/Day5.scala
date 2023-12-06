package day5

@main
def day5(): Unit =
  val input = os.read.lines(os.resource / os.RelPath("day5/input.txt"))
  println(part1(input))
  println(part2(input))

type Seed = Long
case class Conversion(name: String, order: Long, ranges: Seq[SeedRange])
case class SeedRange(start: Seed, end: Seed, adjustment: Long):
  def overlaps(that: SeedRange): Boolean = (start max that.start) <= (end min that.end)
  def merge(that: SeedRange): SeedRange =
    val adj = adjustment + that.adjustment
    SeedRange((start max that.start) + adj, (end min that.end) + adj, 0)

case class Almanac(seeds: Seq[SeedRange], conversions: Seq[Conversion])
object Almanac:
  def parseWithSeedNumbers(str: String): Almanac = parse(str) { seeds =>
    seeds.map(s => SeedRange(s, s, 0)).sortBy(_.start)
  }

  def parseWithSeedRanges(str: String): Almanac = parse(str) { seeds =>
    seeds.sliding(2, 2).map { case Seq(start, length) => SeedRange(start, start + length - 1, 0) }.toSeq.sortBy(_.start)
  }

  private def parse(str: String)(seedsParsingF: Seq[Long] => Seq[SeedRange]): Almanac =
    val Array(seeds, maps*) = str split "\n\n"
    Almanac(
      seeds = seedsParsingF(seeds.replace("seeds: ", "").split(" ").map(_.toLong)),
      conversions = maps.zipWithIndex.map { case (m, index) =>
        val Array(head, tail*) = m split "\n"
        Conversion(
          name = head.replace(" map:", ""),
          order = index,
          ranges = tail
            .map { conversionStr =>
              val Array(dst, src, len) = conversionStr.split(" ").map(_.toLong)
              SeedRange(src, src + len - 1, dst - src)
            }
            .sortBy(_.start)
        )
      }
    )
end Almanac

def convert(seedRange: SeedRange, conversion: Conversion): Seq[SeedRange] =
  val overlappedRanges = conversion.ranges.filter(_.overlaps(seedRange)).map(_.merge(seedRange))
  if overlappedRanges.nonEmpty then overlappedRanges else Seq(seedRange)

def findMin(almanac: Almanac) =
  almanac.conversions
    .foldLeft(almanac.seeds) { case (seedRanges, conversion) =>
      seedRanges.flatMap(convert(_, conversion)).sortBy(_.start)
    }
    .map(seedRange => seedRange.start + seedRange.adjustment)
    .min

def part1(lines: Seq[String]) =
  findMin(Almanac.parseWithSeedNumbers(lines.mkString("\n")))

def part2(lines: Seq[String]) =
  findMin(Almanac.parseWithSeedRanges(lines.mkString("\n")))
