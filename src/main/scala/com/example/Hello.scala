package com.example

import scala.collection.mutable.PriorityQueue
import scala.reflect.ClassTag
import QuadNode._

/**
 *  Only add elements that are within the bounds
 */
case class QuadTree[A](node: QuadNode[A], minX: Float, maxX: Float, minY: Float, maxY: Float) {
  def add(e: Element[A]): QuadTree[A] =
    copy(node = node.add(e, minX, maxX, minY, maxY))
  def inRadiusSquared(r2: Float, x: Float, y: Float)(implicit ord: Ordering[A]) : Array[Element[A]] = {
    node.inRadiusSquared(r2, x, y, minX, maxX, minY, maxY).toArray.flatten
  }
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

sealed trait QuadNode[A] {
  def add(e: Element[A], minX: Float, maxX: Float, minY: Float, maxY: Float) : QuadNode[A]
  final def inRadiusSquared(r2: Float, x: Float, y: Float, minX: Float, maxX: Float, minY: Float, maxY: Float) : Vector[Array[Element[A]]] = {
    val xDist = if (x < minX) square(x - minX) else if (x > maxX) square(x - maxX) else 0
    val yDist = if (y < minY) square(y - minY) else if (y > maxY) square(y - maxY) else 0
    if (xDist + yDist > r2) Vector.empty
    else inRadiusSquaredInside(r2, x, y, minX: Float, maxX: Float, minY: Float, maxY: Float)
  }
  def inRadiusSquaredInside(r2: Float, x: Float, y: Float, minX: Float, maxX: Float, minY: Float, maxY: Float) : Vector[Array[Element[A]]]
}
case class Leaf[A](a: Array[Element[A]]) extends QuadNode[A] {
  def add(e: Element[A], minX: Float, maxX: Float, minY: Float, maxY: Float) : QuadNode[A] =
    if (a.length < maxBinSize) Leaf[A](a :+ e)
    else split(minX, maxX, minY, maxY).add(e, minX, maxX, minY, maxY)

  final def split(minX: Float, maxX: Float, minY: Float, maxY: Float) = {
    val midX = (minX + maxX) * 0.5f
    val midY = (minY + maxY) * 0.5f
    val (lowX, highX) = a.partition(_.x <= midX)
    val (lowXLowY, lowXHighY) = lowX.partition(_.y <= midY)
    val (highXLowY, highXHighY) = highX.partition(_.y <= midY)
    Quad(Leaf(lowXLowY), Leaf(lowXHighY), Leaf(highXLowY), Leaf(highXHighY), midX, midY)
  }

   def inRadiusSquaredInside(r2: Float, x: Float, y: Float, minX: Float, maxX: Float, minY: Float, maxY: Float) : Vector[Array[Element[A]]] = {
     val midX = (minX + maxX) * 0.5f
     val midY = (minY + maxY) * 0.5f
     val farthestX = if (x < midX) maxX else minX
     val farthestY = if (y < midY) maxY else minY
     Vector(if (square(farthestX - x) + square(farthestY - y) < r2) a // This check appears to be worth it
     else a.filter(e => square(e.x - x) + square(e.y - y) <= r2))
   }
}
case class Quad[A](lowXLowY: QuadNode[A], lowXHighY: QuadNode[A], highXLowY: QuadNode[A], highXHighY: QuadNode[A], midX: Float, midY: Float) extends QuadNode[A] {
  def add(e: Element[A], minX: Float, maxX: Float, minY: Float, maxY: Float) : QuadNode[A] = {
    if (e.x <= midX)
      if (e.y <= midY) copy(lowXLowY = lowXLowY.add(e, minX, midX, minY, midY))
      else copy(lowXHighY = lowXHighY.add(e, minX, midX, midY, maxY))
    else
      if (e.y <= midY) copy(highXLowY = highXLowY.add(e, midX, maxX, minY, midY))
      else copy(highXHighY = highXHighY.add(e, midX, maxX, midY, maxY))
  }
  def inRadiusSquaredInside(r2: Float, x: Float, y: Float, minX: Float, maxX: Float, minY: Float, maxY: Float) : Vector[Array[Element[A]]] = {
    lowXLowY.inRadiusSquared(r2, x, y, minX, midX, minY, midY) ++
      lowXHighY.inRadiusSquared(r2, x, y, minX, midX, midY, maxY) ++
      highXLowY.inRadiusSquared(r2, x, y, midX, maxX, minY, midY) ++
      highXHighY.inRadiusSquared(r2, x, y, midX, maxX, midY, maxY)
  }
}

object QuadNode {
  val maxBinSize = 200
  def square(x: Float) : Float = x * x
}
