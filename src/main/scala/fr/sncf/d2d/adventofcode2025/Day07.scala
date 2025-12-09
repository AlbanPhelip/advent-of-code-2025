package fr.sncf.d2d.adventofcode2025

object Day07 extends AdventOfCode {

  override val input: Array[String] = Array(
    ".......S.......",
    "...............",
    ".......^.......",
    "...............",
    "......^.^......",
    "...............",
    ".....^.^.^.....",
    "...............",
    "....^.^...^....",
    "...............",
    "...^.^...^.^...",
    "...............",
    "..^...^.....^..",
    "...............",
    ".^.^.^.^.^...^.",
    "...............",
  )

 override def fileName: String = "aoc07.txt"

  override def execute(): (Long, Long) = {

    val parsedInput = input.zipWithIndex.filter(_._2 % 2 == 0).map(_._1.toCharArray)

    val startIndex = parsedInput.head.zipWithIndex.filter(_._1 == 'S').head._2

    /* Star 1 */
    var star1 = 0L
    parsedInput.tail.foldLeft(Array(startIndex)) { case (tachyonIndexes, line) =>
      tachyonIndexes.flatMap { index =>
        if (line(index) == '^') {
          star1 += 1
          Array(index - 1, index + 1)
        } else {
          Array(index)
        }
      }.distinct
    }

    /* Star 2 */
    // Work with the example but way too slow for the real input
    var star2 = 0L
    def doIt(remainingLines: Array[Array[Char]], index: Int): Unit = {
      if (remainingLines.length == 1) {
        if (remainingLines.head(index) == '^') {
          star2 += 2
        } else {
          star2 += 1
        }
      } else {
        if (remainingLines.head(index) == '^') {
          doIt(remainingLines.tail, index - 1)
          doIt(remainingLines.tail, index + 1)
        } else {
          doIt(remainingLines.tail, index)
        }
      }
    }

    doIt(parsedInput.take(60).tail, startIndex)

    (star1, star2)
  }
}
