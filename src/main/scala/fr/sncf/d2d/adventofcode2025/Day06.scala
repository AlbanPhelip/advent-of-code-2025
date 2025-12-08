package fr.sncf.d2d.adventofcode2025

object Day06 extends AdventOfCode {

  private def executeOperation(nums: Array[Long], operation: String): Long = {
    operation match {
      case "+" => nums.sum
      case "*" => nums.product
    }
  }

 override def fileName: String = "aoc06.txt"

  override def execute(): (Long, Long) = {

    val operations = input.last.split(" ").filter(_.nonEmpty)

    /* Star 1 */
    val numbersStar1 = input.dropRight(1).map(_.split(" ").filter(_.nonEmpty)).transpose

    val star1 =  numbersStar1
      .map(_.map(_.toLong))
      .zip(operations)
      .map { case (nums, operation) => executeOperation(nums, operation) }
      .sum

    /* Star 2 */
    val operationsIndex = input
      .last
      .toCharArray
      .zipWithIndex
      .filter(_._1 != ' ')
      .map(_._2) :+ (input.head.length + 1)

    val numbersStar2 = operationsIndex.sliding(2).map { indexes =>
      input
        .dropRight(1)
        .map(_.slice(indexes(0), indexes(1) - 1))
        .map(_.toCharArray.map(_.toString))
        .transpose
        .map(_.mkString("").trim.toLong)
    }

    val star2 = numbersStar2.zip(operations)
      .map { case (nums, operation) => executeOperation(nums, operation) }
      .sum

    (star1, star2)
  }
}
