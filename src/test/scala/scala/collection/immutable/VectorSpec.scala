package scala.collection
package immutable

import infrastructure._
import org.scalacheck._
import org.scalacheck.util.Buildable

object VectorSpec extends Properties("Vector") with TraversableSpec with SeqSpec {
  
  type CC[X] = Vector[X]

  def create[T](ts: T*): Vector[T] = Vector(ts:_*)
  
  implicit def buildableCC[T]: Buildable[T, Vector] =
    new Buildable[T, Vector] {
      def builder = Vector.newBuilder[T]
    }
}