package lru

import scala.annotation.tailrec


/**
 *
 * Attempt at idiomatic immutable functional implementation
 * */
object LRUCacheImmutable extends App {

  def lruCache(strArray: Array[String]): String = {

    @tailrec
    def helper(index: Int, checker: Map[String, DoublyLinkedList[String]], acc: DoublyLinkedList[String]): DoublyLinkedList[String] = {
      if (index >= strArray.length) acc
      else {
        val string = strArray(index)

        checker.get(string) match {
          case Some(node) =>
            val updatedNode = node.remove(node).append(string)
            helper(index + 1, updateChecker(checker, updatedNode), updatedNode)

          case None =>
            val node = acc.append(string)
            helper(index + 1, updateChecker(checker + (string -> node), node), node)
        }
      }
    }

    @tailrec
    def toString(node: DoublyLinkedList[String], acc: String = ""): String = {
      if (node.isEmpty) acc.reverse
      else if (node.next.isEmpty) node.value + acc.reverse
      else {
        toString(node.next, acc + node.value + "-")
      }
    }

    /** since replacing a single element in an immutable doubly-linked list changes the references of each element/node in the entire list,
     * we have to update references held in the Map
     * */
    @tailrec
    def updateChecker(checker: Map[String, DoublyLinkedList[String]], node: DoublyLinkedList[String]): Map[String, DoublyLinkedList[String]] =
      if (node.isEmpty) checker
      else {
        checker.get(node.value) match {
          case Some(_) => updateChecker(checker + (node.value -> node), node.next)
          case None => updateChecker(checker, node.next)
        }
      }

    toString(helper(0, Map(), Empty))
  }

  val input = Array("A", "B", "C")
  println(lruCache(input))
}
