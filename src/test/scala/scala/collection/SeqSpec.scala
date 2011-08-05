package scala.collection

import infrastructure._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.util.Buildable

trait SeqSpec extends TraversableSpec with TestCollectionOrdered { this: Properties =>

  // Abstract members
  type CC[X] <: Seq[X]

  /* The properties under test */

  property("contains") = forAll { s: CC[Uniq] =>
    !s.contains(new Uniq) :| "does not contain foreign element" &&
    s.forall(u => s.contains(u)) :| "contains every own element"
  }
}