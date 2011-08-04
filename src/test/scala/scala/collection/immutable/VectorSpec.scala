package scala.collection
package immutable

import infrastructure._
import org.scalacheck._
import org.scalacheck.util.Buildable

object VectorSpec extends Properties("Vector") with TraversableSpec {
  
  implicit def buildableTraversable[T]: Buildable[T, collection.Traversable] = new Buildable[T, collection.Traversable] {
    def builder = Vector.newBuilder[T]
  }
  
  def sameElements[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean = sameElementsOrdered(a, b)
}