package com.larroy

import scala.collection.mutable

object AstarPathFinder {
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
 * @param costFunction function that returns the path cost between two adjacent nodes
 * @param heuristicFunction heuristic function for A* (from the origin) to the goal which is given as a parameter, a result of 0 runs pure Dijkstra.
 * @param pqOrd Ordering to be used for the cost in the priority queue
 *
 *
 */
final class AstarPathFinder(
  expand: (Long) => Seq[Long],
  costFunction: (Long, Long) => Double,
  heuristicFunction: (Long) => Double
  )(implicit pqOrd: Ordering[Double]) extends PathFinder {

  override def findPath(from: Long, to: Long): Option[Path] = {
    /// Predecessor of a given node
    var parent = mutable.OpenHashMap.empty[Long, Long]
    // priority queue with (cost, id) ordered by the first element
    var frontier = new mutable.PriorityQueue[(Double, Long)]()(Ordering.by[(Double, Long), Double](_._1).reverse)
    // cost from the from node to the node used as a key
    val costTo = mutable.OpenHashMap.apply[Long, Double]((from -> 0.0))
    // visited nodes, these have been explored already, we won't expand these node nor add them to the frontier again
    val settled = mutable.Set.empty[Long]

    // seed the frontier with the initial node
    frontier += (0.0 -> from)

    while (! frontier.isEmpty) {
      val visiting = frontier.dequeue()._2
      settled += visiting
      expand(visiting).iterator.filter(!settled.contains(_)).foreach { expandedNode =>
        val visitingToExpandedCost = costFunction(visiting, expandedNode)
        val expandedCost = costTo.get(visiting).get + visitingToExpandedCost
        val expandedCostEstimate = expandedCost + heuristicFunction(visiting)
        val oldCost = costTo.get(expandedNode)
        if (oldCost.isEmpty || expandedCostEstimate < oldCost.get + heuristicFunction(expandedNode)) {
          // shortest path found
          costTo += (expandedNode -> expandedCost)
          parent += (expandedNode -> visiting)
          if (expandedNode == to)
            // goal reached
            return new Some(Path(AstarPathFinder.reconstructPath(from, to, parent), expandedCost))
        }
        frontier += (expandedCostEstimate -> expandedNode)
      }
    }
    // no path found
    return None
  }
}
