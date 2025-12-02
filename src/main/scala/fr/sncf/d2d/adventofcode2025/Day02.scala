package fr.sncf.d2d.adventofcode2025

object Day02 extends AdventOfCode {

  private def isIdInvalidStar1(id: Long): Boolean = {
    val length = id.toString.length
    if(length % 2 == 0) {
      id.toString.take(length/2) == id.toString.takeRight(length/2)
    } else {
      false
    }
  }

  private def isIdInvalidStar2(id: Long): Boolean = {
    val stringId = id.toString
    val length = stringId.length

    (1 to length / 2).map { size =>
      if (length % size == 0) {
        stringId.take(size) * (length / size) == stringId
      } else {
        false
      }
    }.exists(identity)
  }

  override def fileName: String = "aoc02.txt"
  override def execute(): (Long, Long) = {

    val parsedInput: Array[Long] = input
      .head
      .split(",")
      .map(_.split("-"))
      .map(l => (l(0).toLong, l(1).toLong))
      .flatMap(x => x._1 to x._2)

    val star1 = parsedInput.filter(isIdInvalidStar1).sum

    val star2 = parsedInput.filter(isIdInvalidStar2).sum

    (star1, star2)
  }
}
