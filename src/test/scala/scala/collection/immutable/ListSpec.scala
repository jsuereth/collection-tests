package scala.collection
package immutable

import infrastructure._
import org.scalacheck._
import org.scalacheck.util.Buildable

object ListSpec extends Properties("List") with TraversableSpec {
  type CC[X] = List[X]

  implicit def buildableCC[T]: Buildable[T, List] =
    new Buildable[T, List] {
      def builder = new mutable.ListBuffer[T]
    }
  
  def sameElements[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean = sameElementsOrdered(a, b)
}
