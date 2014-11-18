package com.larroy

import org.specs2.mutable._
import scala.collection.mutable

/**
 * @author piotr 17.11.14
 */
class AstarPathFinderSpec extends Specification {
  "AstarPathFinderSpec" should {
    "reconstruct a path" in {
      val path = AstarPathFinder.reconstructPath(0L, 4L, mutable.OpenHashMap[Long, Long]((4L->2L), (2L->3L), (3L->1L), (1L->0L)))
      path should beEqualTo(Seq(0,1,3,2,4))
    }

    "find the shortest path" in {
      val graph = Map[Long, Vector[Long]](
        (0L -> Vector(1L, 2L, 3L)),
        (1L -> Vector(4L, 5L)),
        (2L -> Vector(5L)),
        (3L -> Vector(6L)),
        (4L -> Vector(7L)),
        (5L -> Vector(7L)),
        (6L -> Vector(7L))
      )

      val cost = Map[(Long, Long), Double](
        ((0L -> 1L) -> 1),
        ((0L -> 2L) -> 2),
        ((0L -> 3L) -> 3),
        ((1L -> 4L) -> 2),
        ((1L -> 5L) -> 1),
        ((2L -> 5L) -> 2),
        ((3L -> 6L) -> 4),
        ((4L -> 7L) -> 2),
        ((5L -> 7L) -> 1),
        ((6L -> 7L) -> 5)
      )

      def expandNode(x: Long): Seq[Long] = {
        graph.get(x).getOrElse(Seq.empty[Long])
      }

      def costF(from: Long, to: Long): Double = {
        cost.get((from -> to)).getOrElse(0.0)
      }

      val pathFinder = new AstarPathFinder(expandNode, costF, (_: Long) => 0)
      val path07 = pathFinder.findPath(0, 7)
      path07.get.path should beEqualTo(Seq(0L, 1L, 5L, 7L))
      val path45 = pathFinder.findPath(4, 5)
      path45 must beEmpty
    }
  }
}
