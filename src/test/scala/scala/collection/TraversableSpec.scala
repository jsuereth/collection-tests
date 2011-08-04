package scala.collection

import infrastructure._
import org.scalacheck._
import org.scalacheck.Prop._
import org.scalacheck.util.Buildable

trait TraversableSpec { this: Properties =>

  // Abstract members
  implicit def buildableTraversable[T]: Buildable[T, collection.Traversable]
  def sameElements[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean 

  // Concrete members
  val genTraversableOfOnes: Gen[Traversable[Int]] = Gen.containerOf(1)
  
  /* The properties under test */
  
  property("++") = forAll { (a: Traversable[Uniq], b: Traversable[Uniq]) =>
    val ab = (a ++ b)
    (ab.size == a.size + b.size) :| "size adds" &&
    sameElements(ab.take(a.size), a) :| "begins with a" &&
    sameElements(ab.drop(a.size), b) :| "ends with b"
  }
  
  property("/:") = forAll { t: Traversable[Uniq] =>
    val reversed = (List[Uniq]() /: t)((l, u) => u :: l)
    sameElements(t, reversed.reverse) :| "traverses all elements left to right"
  }
  
  property("foldLeft") = forAll { t: Traversable[Uniq] =>
    val reversed = t.foldLeft(List[Uniq]())((l, u) => u :: l)
    sameElements(t, reversed.reverse) :| "traverses all elements left to right"
  }
  
  property(""":\""") = forAll { t: Traversable[Uniq] =>
    val list = (t :\ List[Uniq]())((u, l) => u :: l)
    sameElements(t, list) :| "traverses all elements right to left"
  }

  property("foldRight") = forAll { t: Traversable[Uniq] =>
    val list = t.foldRight(List[Uniq]())((u, l) => u :: l)
    sameElements(t, list) :| "traverses all elements right to left"
  }

  property("""/:\""") = forAll(genTraversableOfOnes) { t: Traversable[Int] =>
    val sum = (t /:\ 0)((a, b) => a + b)
    (sum == t.size) :| "visits all elements"
  }

  property("fold") = forAll(genTraversableOfOnes) { t: Traversable[Int] =>
    val sum = t.fold(0)((a, b) => a + b)
    (sum == t.size) :| "visits all elements"
  }

  property("aggregate") = forAll { t: Traversable[Uniq] =>
    val list = t.aggregate(List[Uniq]())((l, u) => u :: l, (l1, l2) => l1 ::: l2)
    sameElementsUnordered(t, list) :| "visits all elements"
  }

  property("collect") = forAll { t: Traversable[Uniq] =>
    val U = new Uniq
    (t collect { case U => 1 } isEmpty) :| "mismatching elements are not collected" &&
    sameElements(t, t collect { case u: Uniq => u }) :| "matching elements are collected" &&
    (t collect { case u: Uniq => 1 } forall (_ == 1)) :| "partial function is being applied"
  }

  property("collectFirst") = forAll { t: Traversable[Uniq] =>
    val U = new Uniq
    (t collectFirst { case U => 1 } isEmpty) :| "mismatching elements are not considered" &&
    (!t.isEmpty ==> (t.collectFirst{ case u: Uniq => u }.get == t.head)) :| "first matching element is collected" &&
    (!t.isEmpty ==> (t.collectFirst{ case u: Uniq => 1 }.get == 1)) :| "partial function is being applied"
  }

  property("count") = forAll { t: Traversable[Uniq] =>
    (t.count(_ => true) == t.size) :| "count all" &&
    (t.count(_ => false) == 0) :| "count none" && 
    (!t.isEmpty ==> (t.count(_ == t.head) == 1)) :| "count first" 
  }

  property("map") = forAll { t: Traversable[Uniq] => 
    (t.size == t.map(identity).size) :| "preserves size" &&
    sameElements(t, t.map(identity)) :| "xs.map(identity) == xs"
  }
  
  property("map [Int]") = forAll { t: Traversable[Int] => 
    sameElements(t, t.map(_.toString).map(_.toInt)) :| "inverse operations result in equal collection"
  }
}