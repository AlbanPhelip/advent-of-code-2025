package fr.sncf.d2d.adventofcode2025

object Day03 extends AdventOfCode {

  private def getLargestJoltage(numberOfDigit: Int)(line: String): Long = {
    val numbers = line.split("").map(_.toInt).zipWithIndex
    (1 to numberOfDigit).foldLeft(("", numbers)) { case ((value, currentNumbers), i) =>
      val maxValue = currentNumbers.filter(_._2 < (line.length - numberOfDigit + i)).map(_._1).max
      val maxIndex = currentNumbers.filter(_._1 == maxValue).head._2
      (value + maxValue.toString, currentNumbers.filter(_._2 >= maxIndex + 1))
    }._1.toLong
  }

  override def fileName: String = "aoc03.txt"
  override def execute(): (Long, Long) = {

    val star1 = input.map(getLargestJoltage(2)).sum
    val star2 = input.map(getLargestJoltage(12)).sum

    (star1, star2)
  }
}
