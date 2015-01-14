package com.larroy

import org.specs2.mutable.Specification

/**
 * @author larroy 12/16/14
 */
class IndexPQSpec extends Specification {
  "IndexPQ" should {
    "min on empty throws" in {
      val pq = new IndexMinPQ[Long, Double](213)
      pq.min must throwA[NoSuchElementException]
    }

    "insert elements and return the element with the minimum priority" in {
      val pq = new IndexMinPQ[Long, Double](213)
      pq.insert(5, 1.0)
      pq.insert(3, 0.0)
      pq.insert(2, 2.0)
      pq.insert(6, 5.0)
      pq.insert(8, 3.0)
      pq.insert(9, 0.1)
      pq.min shouldEqual((3,0.0))
    }

    "remove the minimum in the right order" in {
      val pq = new IndexMinPQ[Long, Double](213)
      pq.insert(5, 1.0)
      pq.insert(3, 0.0)
      pq.insert(2, 2.0)
      pq.decreaseKey(2, 1.9)
      pq.dequeue() shouldEqual((3, 0.0))
      pq.dequeue() shouldEqual((5, 1.0))
      pq.dequeue() shouldEqual((2, 1.9))
      pq.dequeue() must throwA[NoSuchElementException]
    }

    "reports length" in {
      val pq = new IndexMinPQ[Long, Double](213)
      pq.isEmpty must beTrue
      pq.nonEmpty must beFalse
      pq.length shouldEqual 0

      pq.insert(2, 2.0)

      pq.length shouldEqual 1
      pq.isEmpty must beFalse
      pq.nonEmpty must beTrue
    }

    "access value by key" in {
      val pq = new IndexMinPQ[Long, Double](213)
      val key = 2
      val value = 2.0
      pq(key) must throwA[NoSuchElementException]
      pq.contains(key) must beFalse
      pq.insert(key, value)
      pq(key) shouldEqual value
      pq.contains(key) must beTrue
    }

    "decrease key" in {
      val pq = new IndexMinPQ[Long, Double](213)
      pq.insert(5, 1.0)
      pq.insert(3, 3.0)
      pq.insert(2, 2.0)
      pq.decreaseKey(3, 0.8)
      pq.decreaseKey(2, 0.9)
      pq.dequeue() shouldEqual((3, 0.8))
      pq.dequeue() shouldEqual((2, 0.9))
      pq.dequeue() shouldEqual((5, 1))
    }
  }

  "IndexPQMin regression" should {
    "dequeue in min order" in {
      val pq = new IndexMinPQ[Long, Double](500)
      pq.insert(61051750, 0.0)
      pq.dequeue() shouldEqual((61051750, 0.0))
      pq.decreaseKey(61051010, 221.92979415227595)
      pq.decreaseKey(894522317, 52.79845433124273)
      pq.decreaseKey(61051730, 168.1636573372773)
      pq.decreaseKey(61051006, 74.76001887599428)
      pq.dequeue() shouldEqual ((894522317, 52.79845433124273))
      pq.decreaseKey(61051751, 165.41396662447943)
      pq.dequeue() shouldEqual ((61051006, 74.76001887599428))
      pq.decreaseKey(61050986, 277.46602503181776)
      pq.decreaseKey(61051005, 166.84206745647506)
      pq.dequeue() shouldEqual ((61051751, 165.41396662447943))
      pq.decreaseKey(61051765, 413.21670606952733)
      pq.decreaseKey(61051754, 292.3485455752267)
      pq.dequeue() shouldEqual ((61051005, 166.84206745647506))
      pq.decreaseKey(592111122, 244.35286112167512)
      pq.decreaseKey(61050984, 325.0894072457054)
      pq.decreaseKey(61051729, 336.446856047)
      pq.dequeue() shouldEqual ((61051730, 168.1636573372773))
      pq.decreaseKey(939081657, 176.32585362294117)
      pq.decreaseKey(817062026, 270.2484957263358)
      pq.decreaseKey(817062019, 226.66193823071964)
      pq.dequeue() shouldEqual ((939081657, 176.32585362294117))
      pq.dequeue() shouldEqual ((61051010, 221.92979415227595))
      pq.dequeue() shouldEqual ((817062019, 226.66193823071964))
      pq.dequeue() shouldEqual ((592111122, 244.35286112167512))
      pq.dequeue() shouldEqual ((817062026, 270.2484957263358))
      // This is a violation:

      /*
      pq.dequeue() shouldEqual ((61051010, 221.92979415227595))
      pq.decreaseKey(659912778, 300.9935401848802)
      pq.dequeue() shouldEqual ((592111122, 244.35286112167512))
      pq.decreaseKey(849309187, 274.10896417850034)
      pq.decreaseKey(61050996, 333.6507910938714)
      pq.decreaseKey(849309188, 251.9021230548427)
      pq.dequeue() shouldEqual ((659912778, 300.9935401848802))
      pq.decreaseKey(61051008, 331.69094190767635)
      pq.dequeue() shouldEqual ((61051754, 292.3485455752267))
      */
      //pq.decreaseKey(61051757,438.0234590432699)


    }
  }

}
