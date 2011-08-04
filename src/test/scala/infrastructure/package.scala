// No package declaration for the package object

import java.util.ArrayList

import scala.collection.GenTraversableOnce

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

package object infrastructure {

  val genUniq: Gen[Uniq] = Gen(params => Some(new Uniq))
  implicit val arbUniq: Arbitrary[Uniq] = Arbitrary(genUniq)
  
  // Returns whether the two traversables contain the same elements in the same order
  def sameElementsOrdered[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean = {
    a.toSeq.sameElements(b.toSeq)
  }

  // Returns whether the two traversables contain the same elements in any order
  def sameElementsUnordered[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean = {
    val al = new ArrayList[T]
    a.seq foreach { e => al.add(e) }
    b.seq foreach { e =>
      if(!al.remove(e)) return false
    }
    al.isEmpty
  }
}
