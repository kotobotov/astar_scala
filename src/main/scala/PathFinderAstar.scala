package com.larroy

import scala.collection.mutable
import org.slf4j.{Logger, LoggerFactory}

object PathFinderAstar {
  def reconstructPath(from: Long, to: Long, parent: mutable.Map[Long, Long]): Seq[Long] = {
    val reversed = mutable.ArrayBuffer[Long](to)
    var i = to
    do {
      i = parent.get(i).get
      reversed += i
    } while(i != from)
    reversed.reverse
  }
}


/**
 * @author piotr 17.11.14
 * @param expand function that given a Node returns its direct neighbours
 * @param costFunction function that returns the path cost between two adjacent nodes, (previous, visiting, expanded)
 * @param heuristicFunction heuristic function for A* estimated cost from the current node (the parameter) to the goal, a result of 0 runs pure Dijkstra.
 */
final class PathFinderAstar(
  expand: (Long) => Seq[Long],
  costFunction: (Option[Long], Long, Long) => Double,
  heuristicFunction: (Long) => Double
  ) extends PathFinder {
  case class Stats(var maxFrontier: Int = 0)
  private[this] val log: Logger = LoggerFactory.getLogger(this.getClass)
  var stats = new Stats()

  override def findPath(from: Long, to: Long): Option[Path] = {
    stats = new Stats()
    /// Predecessor of a given node
    val parent = mutable.OpenHashMap.empty[Long, Long]
    // priority queue with (cost, id) ordered by the first element
    //var frontier = new mutable.PriorityQueue[(Double, Long)]()(Ordering.by[(Double, Long), Double](_._1).reverse)
    val frontier = new IndexMinPQ[Long, Double](500)
    // visited nodes, these have been explored already, we won't expand these node nor add them to the frontier again
    val settled = mutable.Set.empty[Long]

    // seed the frontier with the initial node
    //frontier += (0.0 -> from)
    frontier.insert(from, 0.0)

    while (!frontier.isEmpty) {
      val (visiting, visitingCost) = frontier.dequeue()
      log.info(s"Dequeue ${visiting} ${visitingCost}")
      assert(! settled.contains(visiting))
      stats.maxFrontier = Math.max(stats.maxFrontier, frontier.length)
      settled += visiting
      expand(visiting).iterator.filter(!settled.contains(_)).foreach { expandedNode =>
        val visitingToExpandedCost = costFunction(parent.get(visiting), visiting, expandedNode)
        val expandedCost = visitingCost + visitingToExpandedCost
        val expandedCostEstimate = expandedCost + heuristicFunction(visiting)
        val oldCost = frontier.get(expandedNode)
        if (oldCost.isEmpty || expandedCostEstimate < oldCost.get + heuristicFunction(expandedNode)) {
          // shortest path found
          log.info(s"decrease key ${expandedNode} ${expandedCost}")
          frontier.decreaseKey(expandedNode, expandedCost)
          //log.debug(s"Frontier -> ${expandedNode} cost ${expandedCostEstimate}")
          parent += (expandedNode -> visiting)

        }
      }
      if (visiting == to) {
        return new Some(Path(PathFinderAstar.reconstructPath(from, to, parent), 0))
      }
    }
    // no path found
    return None
  }
}
