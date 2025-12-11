package fr.sncf.d2d.adventofcode2025

object Day11 extends AdventOfCode {

  override def fileName: String = "aoc11.txt"

  override def execute(): (Long, Long) = {

    val parsedInput = input.map(_.split(":")).map(l => (l(0), l(1).trim.split(" "))).toMap

    /* Star 1 */
    def findPathStar1(rack: String, count: Long): Long = {
      val nextRacks = parsedInput(rack)
      if (nextRacks.contains("out")) {
        1
      } else {
        nextRacks.map(r => findPathStar1(r, count)).sum
      }
    }

    val star1 = findPathStar1("you", 0L)

    /* Star 2 */
    case class Counter(count: Long, dacFlag: Boolean, fftLag: Boolean) {
      def merge(other: Counter): Counter = Counter(count + other.count, dacFlag || other.dacFlag, fftLag || other.fftLag)
    }

    def findPathStar2(rack: String, counter: Counter): Counter = {
      val newCounter = counter.copy(dacFlag = counter.dacFlag || rack == "dac", fftLag = counter.fftLag || rack == "fft")

      val nextRacks = parsedInput(rack)
      if (nextRacks.contains("out")) {
        if (counter.dacFlag && counter.fftLag) newCounter.copy(count = 1)
        else newCounter
      } else {
        nextRacks.map(r => findPathStar2(r, newCounter)).reduce(_.merge(_))
      }
    }

    // Works with the example but way too slow for the real input
    val star2 = findPathStar2("svr", Counter(0, dacFlag = false, fftLag = false)).count

    (star1, star2)
  }
}
