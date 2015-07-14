package com.boldradius.lambdarange

import com.boldradius.lambdarange.QuadNode._

/**
 *  Only add elements that are within the bounds
 */
case class QuadTree[A](node: QuadNode[A], minX: Float, maxX: Float, minY: Float, maxY: Float) {
  def add(e: Element[A]): QuadTree[A] =
    copy(node = node.add(e, minX, maxX, minY, maxY, 0))

  def inRadius(radius: Float, center: (Float, Float))(implicit ord: Ordering[A]) : Array[Element[A]] =
    node.inRadiusSquared(radius * radius, center._1, center._2, minX, maxX, minY, maxY).toArray.flatten
}

object QuadTree {
  def empty[A](minX: Float, maxX: Float, minY: Float, maxY: Float) : QuadTree[A] =
    QuadTree(Leaf[A](Array.empty), minX, maxX, minY, maxY)

  def apply[A](elements: Seq[Element[A]], minX: Float, maxX: Float, minY: Float, maxY: Float) :  QuadTree[A] =
    elements.foldLeft(empty[A](minX, maxX, minY, maxY))(_.add(_))
}

case class Element[A](a: A, x: Float, y: Float)
