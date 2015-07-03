package com.boldradius.brome

import com.boldradius.brome.QuadNode._

import scala.collection.mutable.PriorityQueue

/**
 *  Only add elements that are within the bounds
 */
case class QuadTree[A](node: QuadNode[A], minX: Float, maxX: Float, minY: Float, maxY: Float) {
  def add(e: Element[A]): QuadTree[A] =
    copy(node = node.add(e, minX, maxX, minY, maxY))
  def inRadius(radius: Float, center: (Float, Float))(implicit ord: Ordering[A]) : Array[Element[A]] =
    node.inRadiusSquared(radius * radius, center._1, center._2, minX, maxX, minY, maxY).toArray.flatten
}

object QuadTree {
  def empty[A](minX: Float, maxX: Float, minY: Float, maxY: Float) : QuadTree[A] =
    QuadTree(Leaf[A](Array.empty), minX, maxX, minY, maxY)
  def apply[A](elements: Seq[Element[A]], minX: Float, maxX: Float, minY: Float, maxY: Float) :  QuadTree[A] =
    elements.foldLeft(empty[A](minX, maxX, minY, maxY))(_.add(_))

  /**
   * This K-way merge should be O(n * log k), but the constant factor is too high
   * so we prefer flatten followed by an O(n * log n) sort.
   */
  @deprecated
  def kWayMerge[A](r: Seq[Seq[Element[A]]])(implicit ord: Ordering[A]) : Array[Element[A]] = {
    type Pair = (Element[A], Seq[Element[A]])
    val queue = new PriorityQueue[Pair]()(new Ordering[Pair] {
      override def compare(x: Pair, y: Pair): Int = ord.compare(y._1.a, x._1.a)
    })
    def enqueue(v: Seq[Element[A]]) : Unit = if (!v.isEmpty) queue += ((v.head, v.tail))
    r.foreach(enqueue)
    val numElements = r.foldLeft(0)(_ + _.length)
    Array.fill[Element[A]](numElements){
      val (element, v) = queue.dequeue()
      enqueue(v)
      element
    }
  }
}

case class Element[A](a: A, x: Float, y: Float)
