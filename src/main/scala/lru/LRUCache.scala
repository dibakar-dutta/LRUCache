package lru

import scala.annotation.tailrec

class LRUCache  {

  private case class Node(value: String, var prev: Option[Node], var next: Option[Node])

  private val size = 5

  private val head: Node = Node("xxx", None, None)
  private val tail: Node = Node("xxx", None, None)

  head.next = Some(tail)
  tail.prev = Some(head)


  private def add(node: Node): Unit = {
    val prev: Option[Node] = tail.prev

    tail.prev = Some(node)
    node.next = Some(tail)

    node.prev = prev
    prev.foreach(_.next = Some(node))
  }

  private def remove(node: Node): Unit = {
    val prev = node.prev
    val next = node.next

    prev.foreach(_.next = next)
    next.foreach(_.prev = prev)
  }

  def lruCache(strArray: Array[String]): String = {
    @tailrec
    def helper(index: Int, checker: Map[String, Node]): Option[Node] = {
      if (index >= strArray.length) {
        head.next.foreach(_.prev = None)
        tail.prev
      } else {
        val string = strArray(index)

        checker.get(string) match {
          case Some(node) =>
            remove(node)
            add(node)
            helper(index + 1, checker)

          case None =>
            val newNode = Node(string, None, None)
            add(newNode)
            val updatedChecker = checker + (string -> newNode)

            // evict least used element from linked list and Map in case exceeding size
            if (updatedChecker.size > size) {
              head.next match {
                case Some(node) =>
                  remove(node)
                  helper(index + 1, updatedChecker - node.value)
                case None =>
                  helper(index + 1, updatedChecker)
              }

            } else
              helper(index + 1, updatedChecker)
        }
      }
    }

    @tailrec
    def toString(node: Option[Node], acc: String = ""): String = {
      node match {
        case None => acc
        case Some(n) =>
          if (n.prev.isEmpty) n.value + acc
          else {
            toString(n.prev, acc = "-" + n.value + acc)
          }
      }
    }

    toString(helper(index = 0, Map()))
  }
}
