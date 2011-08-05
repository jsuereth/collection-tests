package scala.collection
package immutable

import infrastructure._
import org.scalacheck._
import org.scalacheck.util.Buildable

object ListSpec extends Properties("List") with TraversableSpec with SeqSpec {
  
  type CC[X] = List[X]
  
  def create[T](ts: T*): List[T] = List(ts:_*)

  implicit def buildableCC[T]: Buildable[T, List] =
    new Buildable[T, List] {
      def builder = new mutable.ListBuffer[T]
    }
}
