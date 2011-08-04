package infrastructure

import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.Properties

object InfrastructureTests extends Properties("Test Infrastructure") {

  property("sameElementsOrdered") = forAll { is: List[Uniq] =>
    val a, b = new Uniq
    sameElementsOrdered(is, is) :| "same collection" &&
    !sameElementsOrdered(a :: b :: is, b :: a :: is) :| "different order" &&
    !sameElementsOrdered(is, a :: is) :| "different elements"
  }

  property("sameElementsUnordered") = forAll { is: List[Uniq] =>
    val a, b = new Uniq
    sameElementsUnordered(is, is) :| "same collection" &&
    sameElementsUnordered(is, is.reverse) :| "reverse order" &&
    sameElementsUnordered(a :: b :: is, b :: a :: is) :| "different order" &&
    !sameElementsUnordered(is, a :: is) :| "different elements"
  }
}