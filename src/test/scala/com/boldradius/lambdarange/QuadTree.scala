package com.boldradius.lambdarange

import com.boldradius.lambdarange.QuadNode.square
import org.scalatest._

import scala.util.Random

class QuadTreeSpec extends FlatSpec with Matchers {
  "QuadTree" should "return exact set of elements in range" in {
    for (i <- 1 to 30) {
      val r = new Random(12)
      val randomElementStream: Stream[Element[Int]] =
        Stream.continually((r.nextFloat, r.nextFloat)).zipWithIndex.map { case ((x, y), i) => Element(i, x, y) }
      val elements = randomElementStream.take(100000)
      val tree = QuadTree(elements, 0, 1, 0, 1)
      val q@(queryCenter, queryR) = ((r.nextFloat, r.nextFloat), r.nextFloat)
      val result = logTime("query", tree.inRadius(queryR, queryCenter))
//      val sorted= logTime("sort", result.sortBy(_.a))
//      logTime("sort", Sorting.quickSort(result)(new Ordering[Element[Int]] {
//        override def compare(x: Element[Int], y: Element[Int]): Int = x.a - y.a
//      }))
      // This sort appears to be the fastest by a wide margin, and sorting generally takes longer that the query itself
      logTime("sort", java.util.Arrays.sort(result, new Ordering[Element[Int]] {
        override def compare(x: Element[Int], y: Element[Int]): Int = x.a - y.a
      }))
      val sameQueryWithoutQuadTree = elements.filter(e => square(e.x - queryCenter._1) + square(e.y - queryCenter._2) <= queryR * queryR).toVector.sortBy(_.a)
      result should ===(sameQueryWithoutQuadTree)
      println(s"query: $q, found ${result.length} elements")
    }
  }

  def logTime[A](msg: String, a: => A) : A = {
    val start = System.nanoTime()
    val a2 = a
    println(s"$msg took ${(System.nanoTime() - start)/1000} us")
    a2
  }
}
