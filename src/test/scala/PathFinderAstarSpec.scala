package com.larroy

import org.specs2.mutable._
import scala.collection.mutable
import org.slf4j.{Logger, LoggerFactory}

/**
 * @author piotr 17.11.14
 */
class PathFinderAstarSpec extends Specification {
  "AstarPathFinderSpec" should {
    "reconstruct a path" in {
      val path = PathFinderAstar.reconstructPath(0L, 4L, mutable.OpenHashMap[Long, Long]((4L->2L), (2L->3L), (3L->1L), (1L->0L)))
      path should beEqualTo(Seq(0,1,3,2,4))
    }

    "find the shortest path" in {
      val log: Logger = LoggerFactory.getLogger(this.getClass)
      val graph = Map[Long, Vector[Long]](
        (0L -> Vector(1L, 2L, 3L)),
        (1L -> Vector(4L, 5L)),
        (2L -> Vector(1L, 5L, 0L)),
        (3L -> Vector(6L)),
        (4L -> Vector(7L)),
        (5L -> Vector(7L, 1L)),
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
        val res = graph.get(x).getOrElse(Seq.empty[Long])
        log.info(s"expand ${x} ${res}")
        res
      }

      def costF(previous: Option[Long], from: Long, to: Long): Double = {
        log.info(s"cost ${from} -> ${to}")
        cost.get((from -> to)).getOrElse(0.0)
      }

      val pathFinder = new PathFinderAstar(expandNode, costF, (_: Long) => 0)
      val path07 = pathFinder.findPath(0, 7)
      path07.get.path should beEqualTo(Seq(0L, 1L, 5L, 7L))
      val path45 = pathFinder.findPath(4, 5)
      path45 must beEmpty
    }

    "terminating condition" in {
      // make sure we terminate only when the shortest path has settled, in this case the shortest path is 0,1,2,3,4 and not 0,4
      val log: Logger = LoggerFactory.getLogger(this.getClass)
      val graph = Map[Long, Vector[Long]](
        (0L -> Vector(1L, 4L)),
        (1L -> Vector(2L)),
        (2L -> Vector(3L)),
        (3L -> Vector(4L))
      )

      val cost = Map[(Long, Long), Double](
        ((0L -> 1L) -> 1),
        ((1L -> 2L) -> 1),
        ((2L -> 3L) -> 1),
        ((3L -> 4L) -> 1),
        ((0L -> 4L) -> 1000)
      )

      def expandNode(x: Long): Seq[Long] = {
        val res = graph.get(x).getOrElse(Seq.empty[Long])
        log.info(s"expand ${x} ${res}")
        res
      }

      def costF(previous: Option[Long], from: Long, to: Long): Double = {
        log.info(s"cost ${from} -> ${to}")
        cost.get((from -> to)).getOrElse(0.0)
      }

      val pathFinder = new PathFinderAstar(expandNode, costF, (_: Long) => 0)
      val path = pathFinder.findPath(0, 4)
      path.get.path should beEqualTo(Seq(0L, 1L, 2L, 3L, 4L))
    }
  }
  "terminating condition" in {
    // make sure we don't expand more nodes than needed, 5 shouldn't be expanded
    val log: Logger = LoggerFactory.getLogger(this.getClass)
    val graph = Map[Long, Vector[Long]](
      (0L -> Vector(1L, 4L)),
      (1L -> Vector(2L)),
      (2L -> Vector(3L)),
      (3L -> Vector(4L)),
      (4L -> Vector(5L)),
      (5L -> Vector(6L))
    )

    val cost = Map[(Long, Long), Double](
      ((0L -> 1L) -> 1),
      ((1L -> 2L) -> 1),
      ((2L -> 3L) -> 1),
      ((3L -> 4L) -> 1),
      ((0L -> 4L) -> 1000),
      ((4L -> 5L) -> 1),
      ((5L -> 6L) -> 1)
    )

    def expandNode(x: Long): Seq[Long] = {
      val res = graph.get(x).getOrElse(Seq.empty[Long])
      log.info(s"expand ${x} ${res}")
      // we don't expand more than needed
      x shouldNotEqual 5
      res
    }

    def costF(previous: Option[Long], from: Long, to: Long): Double = {
      log.info(s"cost ${from} -> ${to}")
      cost.get((from -> to)).getOrElse(0.0)
    }

    val pathFinder = new PathFinderAstar(expandNode, costF, (_: Long) => 0)
    val path = pathFinder.findPath(0, 4)
    path.get.path should beEqualTo(Seq(0L, 1L, 2L, 3L, 4L))
  }

   "disconnected graph" in {
    // make sure we don't expand more nodes than needed, 5 shouldn't be expanded
    val log: Logger = LoggerFactory.getLogger(this.getClass)
    val graph = Map[Long, Vector[Long]](
      (0L -> Vector(1L)),
      (1L -> Vector(2L)),
      (2L -> Vector(2L)),
      (3L -> Vector(4L)),
      (4L -> Vector(5L)),
      (5L -> Vector(6L))
    )

    val cost = Map[(Long, Long), Double](
      ((0L -> 1L) -> 1),
      ((1L -> 2L) -> 1),
      ((2L -> 3L) -> 1),
      ((3L -> 4L) -> 1),
      ((4L -> 5L) -> 1),
      ((5L -> 6L) -> 1)
    )

    def expandNode(x: Long): Seq[Long] = {
      val res = graph.get(x).getOrElse(Seq.empty[Long])
      log.info(s"expand ${x} ${res}")
      // we don't expand more than needed
      x shouldNotEqual 5
      res
    }

    def costF(previous: Option[Long], from: Long, to: Long): Double = {
      log.info(s"cost ${from} -> ${to}")
      cost.get((from -> to)).getOrElse(0.0)
    }

    val pathFinder = new PathFinderAstar(expandNode, costF, (_: Long) => 0)
    val path = pathFinder.findPath(0, 4)
    path should beNone
  }
}
