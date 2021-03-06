package com.boldradius.lambdarange.sort
import com.boldradius.lambdarange.Element
import math.sqrt
import com.boldradius.lambdarange.QuadNode.square
import scala.collection.breakOut

case class SquaredDistance[A](squaredDistance: Float, element: A) {
  def distance = sqrt(squaredDistance)
}

object SquaredDistanceSort {
  def elementSquaredDistances[A](elements: Seq[Element[A]], center: (Float, Float)) : Array[SquaredDistance[Element[A]]] =
    elements.map[SquaredDistance[Element[A]], Array[SquaredDistance[Element[A]]]](e => SquaredDistance(square(e.x - center._1) + square(e.y - center._2), e))(breakOut)

  def sortElements[A](elements: Seq[Element[A]], center: (Float, Float)) : Array[SquaredDistance[Element[A]]] = {
    val squaredDistances = elementSquaredDistances(elements, center)
    java.util.Arrays.sort(squaredDistances, new Ordering[SquaredDistance[Element[A]]] {
      override def compare(x: SquaredDistance[Element[A]], y: SquaredDistance[Element[A]]): Int = {
        val xs = x.squaredDistance
        val ys = y.squaredDistance
        if (xs < ys) -1 else if (xs > ys) 1 else 0
      }
    })
    squaredDistances
  }


}

