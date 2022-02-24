package lru

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class LRUCacheSpec extends AnyWordSpec with Matchers {

  "LRUCache" should {
    "give valid output of 5 elements for an non-empty large size Array" in {
      val input = Array("A", "B", "C", "D", "A", "E", "D", "Z")
      val result = new LRUCache().lruCache(input)

      result.length shouldEqual 9  // with 4 hyphens in between and 5 values from cache
      result shouldEqual "C-A-E-D-Z"
    }

    "give valid output of 2 elements for an smaller size Array" in {
      val input = Array("A", "B", "A")
      val result = new LRUCache().lruCache(input)

      result.length shouldEqual 3
      result shouldEqual "B-A"
    }

    "give valid output for empty Array" in {
      val input = Array[String]()
      val result = new LRUCache().lruCache(input)

      result shouldEqual ""
    }
  }
}
