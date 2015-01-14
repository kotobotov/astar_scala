package com.larroy

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * @author larroy 12/1/14
 */
class IndexMinPQ[Key, Value](initialSize: Int)(implicit ord: Ordering[Value]) {

  /**
   * @param value The external Value type
   * @param index index in the binary heap
   */
  sealed case class Node(var value: Value, var index: Int)
  val index = new mutable.OpenHashMap[Key, Node](initialSize)
  // idx 0 is not used, bitcount is level, root idx is 1, left sibling 10, right 11, and so on
  val heap = new ArrayBuffer[Key](initialSize + 1)
  //def defaultValue[U]: U = { class Default[U] {var default: U = _ }; new Default[U].default }
  //heap += defaultValue[Key] // dummy element
  //heap += 0.asInstanceOf[Key] null causes intellij not to display heap correctly
  heap += null.asInstanceOf[Key]


  def length: Int = heap.length - 1
  def isEmpty: Boolean = length == 0
  def nonEmpty: Boolean = length != 0

  private[this] val rootIndex = 1
  private[this] def parentIndex(index: Int): Int = {
    require(index > 0)
    require(index < heap.size)
    index >>> 1
  }
  private[this] def leftIndex(index: Int): Int = {
    (index << 1)
  }
  private[this] def rightIndex(index: Int): Int = (index << 1) + 1
  private[this] def leftSiblingIndex(index: Int) = {
    require((index & 1) == 1)
    index - 1
  }
  private[this] def rightSiblingIndex(index: Int) = {
    require((index & 1) == 0)
    index + 1
  }

  private[this] def getNodeAt(extract: (Int) => Int, idx: Int): Option[Node] = {
    val target = extract(idx)
    if (target < heap.size) {
      val nodeOpt = index.get(heap(target))
      assert(nodeOpt.nonEmpty)
      nodeOpt
    } else
      None
  }

  private[this] def parentHeap(node: Node): Option[Node] = {
    node.index match {
      case 1 => None
      case _ @ idx => index.get(heap(parentIndex(idx)))
    }
  }
  private[this] def leftHeap(node: Node): Option[Node] = getNodeAt(leftIndex, node.index)
  private[this] def rightHeap(node: Node): Option[Node] = getNodeAt(rightIndex, node.index)
  private[this] def leftSiblingHeap(node: Node): Option[Node] = getNodeAt(leftSiblingIndex, node.index)
  private[this] def rightSiblingHeap(node: Node): Option[Node] = getNodeAt(rightSiblingIndex, node.index)

  /**
   * Swap two nodes in the heap and changing the respective Node indices
   */
  def swapHeap(from: Node, to: Node): Unit = {
    swapKey(from.index, to.index)
    // swap index
    val oldToIdx = to.index
    to.index = from.index
    from.index = oldToIdx
  }

  private[this] def swapKey(aIdx: Int, bIdx: Int): Unit = {
    val toKey = heap(bIdx)
    heap(bIdx) = heap(aIdx)
    heap(aIdx) = toKey
  }

  @tailrec
  private[this] def downHeap(node: Node): Unit = {
    val left = leftHeap(node)
    if (left.nonEmpty) {
      val right = rightHeap(node)
      val target = if (right.nonEmpty && ord.lt(right.get.value, left.get.value))  right.get else left.get
      if (ord.lt(target.value, node.value)) {
        swapHeap(node, target)
        downHeap(node)
      }
      // else we found our place
    }
    // else we found our place
  }

  @tailrec
  private[this] def upHeap(node: Node): Unit = parentHeap(node) match {
    case Some(parent) if (ord.lt(node.value, parent.value)) => {
      // restore heap property with parent
      swapHeap(node, parent)
      upHeap(node)
    }
    case None => // this is the root, stop
    case _ => // property heap holds, stop
  }

  def assertHeap: Unit = {
    heap.iterator.zipWithIndex.drop(2).foreach { x =>
      val idx = x._2
      val k = x._1
      val node: Node = index(k)
      if (ord.gt(parentHeap(node).get.value, node.value))
        assert(false)
    }
  }

  def decreaseKey(key: Key, value: Value): Unit = {
    index.get(key) match {
      case Some(node) => if (node.value != value) {
        require(ord.lt(value, node.value))
        node.value = value
        upHeap(node)
      }
      case None => insert(key, value)
    }
  }

  def increaseKey(key: Key, value: Value): Unit = index.get(key) match {
    case Some(node) => if (node.value != value) {
      require(ord.gt(value, node.value))
      node.value = value
      downHeap(node)
    }
    case None => insert(key, value)
  }

  def changeKey(key: Key, value: Value): Unit = index.get(key) match {
    case Some(node) if (node.value != value) => {
      node.value = value
      if (ord.gt(value, node.value))
        downHeap(node)
      else
        upHeap(node)
    }
    case None => insert(key, value)
  }

  def insert(key: Key, value: Value): Unit = {
    require(index.get(key).isEmpty)
    heap += key
    val node = new Node(value, heap.size - 1)
    index += (key -> node)
    upHeap(node)
  }

  def min: (Key, Value) = {
    if (heap.size <= 1)
      throw new NoSuchElementException
    val key = heap(rootIndex)
    (key, index(key).value)
  }

  def removeMin(): Unit = {
    // move min to last
    swapHeap(index(heap(rootIndex)), index(heap.last))

    // remove last
    index.remove(heap.last)
    heap.reduceToSize(heap.size - 1)

    // restore heap property
    if (nonEmpty)
      downHeap(index(heap(rootIndex)))
  }

  def dequeue(): (Key, Value) = {
    val res = min
    removeMin
    res
  }

  def apply(k: Key): Value = index(k).value
  def contains(k: Key): Boolean = index.contains(k)
  def get(k: Key): Option[Value] = index.get(k).map(_.value)
}
