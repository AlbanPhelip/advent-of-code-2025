package fr.sncf.d2d.adventofcode2025

import scala.util.{Success, Try}

object Day04 extends AdventOfCode {

  val parsedInput: Array[Array[Char]] = input.map(_.toCharArray)

  private def countAdjacent(x: Int, y: Int): Int = {
    Array(
      Try(parsedInput(y - 1)(x)),
      Try(parsedInput(y + 1)(x)),
      Try(parsedInput(y - 1)(x - 1)),
      Try(parsedInput(y + 1)(x - 1)),
      Try(parsedInput(y - 1)(x + 1)),
      Try(parsedInput(y + 1)(x + 1)),
      Try(parsedInput(y)(x - 1)),
      Try(parsedInput(y)(x + 1))
    ).count(_ == Success('@'))
  }

  override def fileName: String = "aoc04.txt"

  override def execute(): (Long, Long) = {

    /* Star 1 */
    val star1 = parsedInput.indices.flatMap { y =>
      parsedInput.head.indices.filter { x =>
        parsedInput(y)(x) == '@' && countAdjacent(x, y) < 4
      }
    }.length

    /* Star 2 */
    var finished = false
    var star2 = 0

    while(!finished) {
      var localCount = 0
      parsedInput.indices.foreach { y =>
        parsedInput.head.indices.foreach { x =>
          if(parsedInput(y)(x) == '@' && countAdjacent(x, y) < 4) {
            localCount += 1
            parsedInput(y)(x) = '.'
          }
        }
      }
      star2 += localCount
      if (localCount == 0) finished = true
    }

    (star1, star2)
  }
}
