package fr.sncf.d2d.adventofcode2025

import scala.math.abs

object Day09 extends AdventOfCode {
/*
  override val input: Array[String] = Array(
    "7,1",
    "11,1",
    "11,7",
    "9,7",
    "9,5",
    "2,5",
    "2,3",
    "7,3",
  )
*/
  override def fileName: String = "aoc09.txt"

  private case class Point(x: Long, y: Long)

  override def execute(): (Long, Long) = {

    val parsedInput = input.map(_.split(",")).map(l => Point(l(0).toInt, l(1).toInt))

    val cross = for {
      a <- parsedInput
      b <- parsedInput
    } yield (a, b)

    /* Star 1 */
    val star1 = cross.map { case (p1, p2) => (abs(p2.x - p1.x) + 1) * abs((p2.y - p1.y) + 1) }.max

    /* Star 2 */
    val star2 = 0L

    (star1, star2)
  }
}
