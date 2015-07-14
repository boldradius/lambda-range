package com.boldradius.lambdarange

import QuadNode._

sealed trait QuadNode[A] {
  def add(e: Element[A], minX: Float, maxX: Float, minY: Float, maxY: Float, depth: Int) : QuadNode[A]
  final def inRadiusSquared(r2: Float, x: Float, y: Float, minX: Float, maxX: Float, minY: Float, maxY: Float) : Vector[Array[Element[A]]] = {
    val xDist = if (x < minX) square(x - minX) else if (x > maxX) square(x - maxX) else 0
    val yDist = if (y < minY) square(y - minY) else if (y > maxY) square(y - maxY) else 0
    if (xDist + yDist > r2) Vector.empty
    else inRadiusSquaredInside(r2, x, y, minX: Float, maxX: Float, minY: Float, maxY: Float)
  }
  def inRadiusSquaredInside(r2: Float, x: Float, y: Float, minX: Float, maxX: Float, minY: Float, maxY: Float) : Vector[Array[Element[A]]]
}
case class Leaf[A](a: Array[Element[A]]) extends QuadNode[A] {
  def add(e: Element[A], minX: Float, maxX: Float, minY: Float, maxY: Float, depth: Int) : QuadNode[A] =
    if (a.length < maxBinSize || depth > maxDepth) Leaf[A](a :+ e)
    else split(minX, maxX, minY, maxY).add(e, minX, maxX, minY, maxY, depth)

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
    Vector(if (square(farthestX - x) + square(farthestY - y) < r2) a // TODO: move this test to QuadNode level
    else a.filter(e => square(e.x - x) + square(e.y - y) <= r2))
  }
}
case class Quad[A](lowXLowY: QuadNode[A], lowXHighY: QuadNode[A], highXLowY: QuadNode[A], highXHighY: QuadNode[A], midX: Float, midY: Float) extends QuadNode[A] {
  def add(e: Element[A], minX: Float, maxX: Float, minY: Float, maxY: Float, depth: Int) : QuadNode[A] = {
    if (e.x <= midX)
      if (e.y <= midY) copy(lowXLowY = lowXLowY.add(e, minX, midX, minY, midY, depth+1))
      else copy(lowXHighY = lowXHighY.add(e, minX, midX, midY, maxY, depth+1))
    else
    if (e.y <= midY) copy(highXLowY = highXLowY.add(e, midX, maxX, minY, midY, depth+1))
    else copy(highXHighY = highXHighY.add(e, midX, maxX, midY, maxY, depth+1))
  }
  def inRadiusSquaredInside(r2: Float, x: Float, y: Float, minX: Float, maxX: Float, minY: Float, maxY: Float) : Vector[Array[Element[A]]] = {
    lowXLowY.inRadiusSquared(r2, x, y, minX, midX, minY, midY) ++
      lowXHighY.inRadiusSquared(r2, x, y, minX, midX, midY, maxY) ++
      highXLowY.inRadiusSquared(r2, x, y, midX, maxX, minY, midY) ++
      highXHighY.inRadiusSquared(r2, x, y, midX, maxX, midY, maxY)
  }
}

object QuadNode {
  private[lambdarange] val maxBinSize = 200
  private[lambdarange] val maxDepth = 14
  private[lambdarange] def square(x: Float) : Float = x * x
}
