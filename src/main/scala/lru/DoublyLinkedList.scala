package lru

import scala.annotation.tailrec


/**
 * immutable implementation of doubly-linked list
 * */
trait DoublyLinkedList[+T] {
  def value: T
  def prev: DoublyLinkedList[T]
  def next: DoublyLinkedList[T]

  def isEmpty: Boolean

  def prepend[S >: T](element: S): DoublyLinkedList[S]
  def append[S >: T](element: S): DoublyLinkedList[S]
  def remove[S >: T](nodeToRemove: DoublyLinkedList[S]): DoublyLinkedList[S]
  def goToHead: DoublyLinkedList[T]

  def updatePrev[S >: T](newPrev: => DoublyLinkedList[S]): DoublyLinkedList[S]
  def updateNext[S >: T](newNext: => DoublyLinkedList[S]): DoublyLinkedList[S]
}

object Empty extends DoublyLinkedList[Nothing] {
  override def value: Nothing = throw new NoSuchElementException
  override def prev: DoublyLinkedList[Nothing] = throw new NoSuchElementException
  override def next: DoublyLinkedList[Nothing] = throw new NoSuchElementException

  override def isEmpty: Boolean = true

  override def prepend[S >: Nothing](element: S): DoublyLinkedList[S] = new Cons(element, Empty, Empty)
  override def append[S >: Nothing](element: S): DoublyLinkedList[S] = new Cons(element, Empty, Empty)
  override def remove[S >: Nothing](nodeToRemove: DoublyLinkedList[S]): DoublyLinkedList[S] = this
  override def goToHead: DoublyLinkedList[Nothing] = this

  override def updatePrev[S >: Nothing](newPrev: => DoublyLinkedList[S]): DoublyLinkedList[S] = this
  override def updateNext[S >: Nothing](newNext: => DoublyLinkedList[S]): DoublyLinkedList[S] = this
}

class Cons[T](val value: T, p: => DoublyLinkedList[T], n: => DoublyLinkedList[T]) extends DoublyLinkedList[T] {
  override lazy val prev: DoublyLinkedList[T] = p
  override lazy val next: DoublyLinkedList[T] = n

  override def isEmpty: Boolean = false

  override def updatePrev[S >: T](newPrev: => DoublyLinkedList[S]): DoublyLinkedList[S] = {
    lazy val result: DoublyLinkedList[S] = new Cons(value, newPrev, n.updatePrev(result))
    result
  }

  override def updateNext[S >: T](newNext: => DoublyLinkedList[S]): DoublyLinkedList[S] = {
    lazy val result: DoublyLinkedList[S] = new Cons(value, p.updateNext(result), newNext)
    result
  }

  override def append[S >: T](element: S): DoublyLinkedList[S] = {
    lazy val result: DoublyLinkedList[S] = new Cons(value, p.updateNext(result), n.append(element).updatePrev(result))
    result
  }

  override def prepend[S >: T](element: S): DoublyLinkedList[S] = {
    lazy val result: DoublyLinkedList[S] = new Cons(value, p.prepend(element).updateNext(result), n.updatePrev(result))
    result.goToHead
  }

  /**
   * this method is bugged
   * */
  override def remove[S >: T](nodeToRemove: DoublyLinkedList[S]): DoublyLinkedList[S] = {
    val prevNode = nodeToRemove.prev
    val nextNode = nodeToRemove.next

    @tailrec
    def helper(list: DoublyLinkedList[S], node: DoublyLinkedList[S]): DoublyLinkedList[S] = {
      if (node.isEmpty) list
      else helper(list.append(node.value), node.next)
    }

    helper(prevNode, nextNode).goToHead
  }

  override def goToHead: DoublyLinkedList[T] = {
    def helper(list: DoublyLinkedList[T]): DoublyLinkedList[T] =
      if (list.prev.isEmpty) list
      else helper(list.prev)

    helper(this)
  }
}

