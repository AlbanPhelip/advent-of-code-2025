package fr.sncf.d2d.adventofcode2025

object Day01 extends AdventOfCode {

  private val startPosition = 50

  private def instructionToNumber(instruction: String): Int = {
    instruction.head match {
      case 'L' => -1 * instruction.tail.toInt
      case 'R' => instruction.tail.toInt
    }
  }

  override def fileName: String = "aoc01.txt"
  override def execute(): (Long, Long) = {

    val star1 = input
      .map(instructionToNumber)
      .foldLeft((startPosition, 0)){ case ((position, count), instruction) =>
        val newPosition = (position + instruction) % 100
        val newCount = if (newPosition == 0) count + 1 else count
        val finalPosition = if (newPosition < 0) 100 + newPosition else newPosition
        (finalPosition, newCount)
      }._2

    val star2 = input
      .map(instructionToNumber)
      .foldLeft((startPosition, 0)){ case ((position, count), instruction) =>
        val newPosition = (position + instruction) % 100

        val currentCount = if (instruction + position == 0) 1
        else if((instruction + position) < 0) {
          (if(position == 0) 0 else 1) - (instruction + position) / 100
        }
        else (position + instruction) / 100

        val finalPosition = if (newPosition < 0) 100 + newPosition else newPosition
        (finalPosition, count + currentCount)
      }._2

    (star1, star2)
  }
}
