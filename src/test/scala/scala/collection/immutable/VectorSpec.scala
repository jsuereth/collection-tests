package scala.collection
package immutable

import infrastructure._
import org.scalacheck._
import org.scalacheck.util.Buildable

object VectorSpec extends Properties("Vector") with TraversableSpec {
  type CC[X] = Vector[X]

  implicit def buildableCC[T]: Buildable[T, Vector] =
    new Buildable[T, Vector] {
      def builder = Vector.newBuilder[T]
    }
  
  def sameElements[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean = sameElementsOrdered(a, b)
}