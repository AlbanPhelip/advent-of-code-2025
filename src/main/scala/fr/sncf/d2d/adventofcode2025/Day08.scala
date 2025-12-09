package fr.sncf.d2d.adventofcode2025

import scala.math.{sqrt, pow}

object Day08 extends AdventOfCode {

  override def fileName: String = "aoc08.txt"

  private case class Point(x: Long, y: Long, z: Long)

  private def distance(p1: Point, p2: Point): Double = {
    sqrt(pow(p2.x - p1.x, 2) + pow(p2.y - p1.y, 2) + pow(p2.z - p1.z, 2))
  }

  override def execute(): (Long, Long) = {

    val parsedInput = input.map(_.split(",")).map(l => Point(l(0).toLong, l(1).toLong, l(2).toLong))

    val cross =
      for {
        (x, i) <- parsedInput.zipWithIndex
        (y, j) <- parsedInput.zipWithIndex
        if j > i
      } yield (x, y)

    val connexions = cross.map(points => (points, distance(points._1, points._2))).sortBy(_._2).map(_._1)


    var clusters: Array[Array[Point]] = Array(Array(connexions.head._1, connexions.head._2))

    def computeClustersStar1(p1: Point, p2: Point): Unit = {
      val res = clusters.zipWithIndex.filter { case (points, _) =>
        points.contains(p1) || points.contains(p2)
      }

      if (res.length == 2) {
        val oldClusters = clusters.zipWithIndex.filter(x => res.map(_._2).contains(x._2)).map(_._1)
        clusters = clusters.zipWithIndex.filter(x => !res.map(_._2).contains(x._2)).map(_._1)
        clusters = clusters :+ (oldClusters(0) ++ oldClusters(1))

      } else if (res.length == 1) {
        clusters(res.head._2) = (res.head._1 ++ Array(p1, p2)).distinct
      } else if (res.length == 0) {
        clusters = clusters :+ Array(p1, p2)
      }
    }

    var star2 = 0L

    def computeClustersStar2(p1: Point, p2: Point): Boolean = {
      computeClustersStar1(p1, p2)

      if (clusters.length == 1 && clusters.head.length == input.length) {
        star2 = p1.x * p2.x
        true
      } else {
        false
      }
    }

    /* Star 1 */
    connexions.take(1000).tail.foreach { case (p1, p2) => computeClustersStar1(p1, p2) }
    val star1 = clusters.map(_.length).sorted.reverse.take(3).product

    /* Star 2 */
    clusters = Array(Array(connexions.head._1, connexions.head._2))
    connexions.tail.collectFirst { case (p1, p2) if computeClustersStar2(p1, p2) => () }

    (star1, star2)
  }
}
