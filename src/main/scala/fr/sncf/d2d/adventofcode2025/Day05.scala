package fr.sncf.d2d.adventofcode2025


object Day05 extends AdventOfCode {


  val parsedInput: Array[String] = input.mkString(";").split(";;")
  private val ranges: Array[(Long, Long)] = parsedInput.head.split(";").map(_.split("-")).map(x => (x(0).toLong, x(1).toLong))
  private val ingredients: Array[Long] = parsedInput(1).split(";").map(_.toLong)

  override def fileName: String = "aoc05.txt"

  override def execute(): (Long, Long) = {

    /* Star 1 */
    val star1 = ingredients.count { ingredient =>
      ranges.exists { case (min, max) => ingredient >= min && ingredient <= max }
    }

    /* Star 2 */
    val star2 =
      ranges
        .sortBy(_._1)
        .foldLeft(List.empty[(Long, Long)]) {
          case (Nil, curr) =>
            List(curr)

          case (acc @ (start, end) :: rest, (s, e)) =>
            if (s <= end) {
              (start, math.max(end, e)) :: rest
            } else {
              (s, e) :: acc
            }
        }
        .map { case (min, max) => max - min + 1 }
        .sum

    (star1, star2)
  }
}
