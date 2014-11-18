package com.larroy


/**
 * @author piotr 17.11.14
 */
trait PathFinder {
  /**
   * @param from id of the source
   * @param to id of the destination
   * @return an optional map of predecessor ids if a path is found
   */
  def findPath(from: Long, to: Long): Option[Path] = ???
}
