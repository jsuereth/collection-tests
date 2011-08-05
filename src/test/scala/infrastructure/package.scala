// No package declaration for the package object

import java.util.ArrayList

import scala.collection.GenTraversableOnce
import scala.math.max
import scala.math.min

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

package object infrastructure {

  val genUniq: Gen[Uniq] = Gen(params => Some(new Uniq))
  implicit val arbUniq: Arbitrary[Uniq] = Arbitrary(genUniq)
  
  def clamp(n: Int, lower: Int, upper: Int): Int = max(lower, min(upper, n))
  
  // Returns whether the two traversables contain the same elements in the same order
  def sameElementsOrdered[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean = {
    a.toSeq.sameElements(b.toSeq)
  }

  // Returns whether all elements from contained are contained in container in the same order
  def containsAllInOrder[T](container: GenTraversableOnce[T], contained: GenTraversableOnce[T]): Boolean = {
    container.toSeq.seq.containsSlice(contained.toSeq)
  }

  // Returns whether the two traversables contain the same elements in any order
  def sameElementsUnordered[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean = {
    containsAllElements(a, b, sizeMayDiffer = false)
  }

  // Returns whether all elements from contained are contained in container in any order
  def containsAll[T](container: GenTraversableOnce[T], contained: GenTraversableOnce[T]): Boolean = {
    containsAllElements(container, contained, sizeMayDiffer = true)
  }

  // Returns whether all elements from contained are contained in container, in any order
  private def containsAllElements[T](container: GenTraversableOnce[T],
                                     contained: GenTraversableOnce[T],
                                     sizeMayDiffer: Boolean): Boolean = {
    val al = new ArrayList[T]
    container.seq foreach { e => al.add(e) }
    contained.seq foreach { e => if(!al.remove(e)) return false }
    sizeMayDiffer || al.isEmpty
  }
}
