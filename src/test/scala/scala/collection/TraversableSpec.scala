package scala.collection

import java.util.concurrent.atomic.AtomicInteger
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.propBoolean
import org.scalacheck.util.Buildable
import org.scalacheck.Gen
import org.scalacheck.Properties
import infrastructure.arbUniq
import infrastructure.clamp
import infrastructure.sameElementsUnordered
import infrastructure.TestCollection
import infrastructure.Uniq
import infrastructure.UniqSub

trait TraversableSpec extends TestCollection { this: Properties =>

  // Abstract members
  type CC[X] <: Traversable[X]

  // Concrete members
  val genTraversableOfOnes: Gen[Traversable[Int]] = Gen.containerOf(1)
  
  /* The properties under test */
  
  property("++") = forAll { (a: CC[Uniq], b: CC[Uniq]) =>
    val ab = (a ++ b)
    (ab.size == a.size + b.size) :| "size adds" &&
    sameElements(ab.take(a.size), a) :| "begins with a" &&
    sameElements(ab.drop(a.size), b) :| "ends with b"
  }
  
  property("/:") = forAll { t: CC[Uniq] =>
    val reversed = (List[Uniq]() /: t)((l, u) => u :: l)
    sameElements(t, reversed.reverse) :| "traverses all elements left to right"
  }
  
  property("foldLeft") = forAll { t: CC[Uniq] =>
    val reversed = t.foldLeft(List[Uniq]())((l, u) => u :: l)
    sameElements(t, reversed.reverse) :| "traverses all elements left to right"
  }
  
  property(""":\""") = forAll { t: CC[Uniq] =>
    val list = (t :\ List[Uniq]())((u, l) => u :: l)
    sameElements(t, list) :| "traverses all elements right to left"
  }

  property("foldRight") = forAll { t: CC[Uniq] =>
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

  property("aggregate") = forAll { t: CC[Uniq] =>
    val list = t.aggregate(List[Uniq]())((l, u) => u :: l, (l1, l2) => l1 ::: l2)
    sameElementsUnordered(t, list) :| "visits all elements"
  }

  property("collect") = forAll { t: CC[Uniq] =>
    val U = new Uniq
    (t collect { case U => 1 } isEmpty) :| "mismatching elements are not collected" &&
    sameElements(t, t collect { case u: Uniq => u }) :| "matching elements are collected" &&
    (t collect { case u: Uniq => 1 } forall (_ == 1)) :| "partial function is being applied"
  }

  property("collectFirst") = forAll { t: CC[Uniq] =>
    val U = new Uniq
    (t collectFirst { case U => 1 } isEmpty) :| "mismatching elements are not considered" &&
    (t.collectFirst{ case u: Uniq => u } == t.headOption) :| "first matching element is collected" &&
    (t.collectFirst{ case u: Uniq => 1 } == t.headOption.map(_ => 1)) :| "partial function is being applied"
  }

  property("count") = forAll { t: CC[Uniq] =>
    (t.count(_ => true) == t.size) :| "count all" &&
    (t.count(_ => false) == 0) :| "count none" && 
    (t.forall(e => t.count(_ == e) == 1)) :| "count any element" 
  }

  property("drop") = forAll { (t: CC[Uniq], n: Int) =>
    val size = t.size
    val resultSize = size - clamp(n, 0, size)
    val dropped = t.drop(n)
    (dropped.size == resultSize) :| "right number of elements dropped" &&
    containsSlice(t, dropped) :| "remaining elements from original collection" &&
    sameElements(t, t.drop(0)) :| "drop none" &&
    t.drop(Int.MaxValue).isEmpty :| "drop all"
  }

  property("dropWhile") = forAll { (t: CC[Uniq], n: Int) =>
    (t.dropWhile(_ => true).isEmpty) :| "drop all" &&
    sameElements(t, t.dropWhile(_ => false)) :| "drop none"
  }

  property("exists") = forAll { t: CC[Uniq] =>
    !t.exists(_ => false) :| "exists false" &&
    (t.exists(_ => true) != t.isEmpty) :| "exists true" &&
    (t.forall { e => t.exists(_ == e) }) :| "every elements exists"
  }

  property("filter") = forAll { t: CC[Uniq] =>
    t.filter(_ => false).isEmpty :| "filter all" &&
    sameElements(t, t.filter(_ => true)) :| "filter none" &&
    (t filter { case _: UniqSub => true case _ => false } isEmpty) :| "filter all (subclass)"
  }

  property("filterNot") = forAll { t: CC[Uniq] =>
    t.filterNot(_ => true).isEmpty :| "filter all" &&
    sameElements(t, t.filterNot(_ => false)) :| "filter none" &&
    (t filterNot { case _: UniqSub => false case _ => true } isEmpty) :| "filter all (subclass)"
  }

  property("find") = forAll { t: CC[Uniq] =>
    (t.find(_ => true).isEmpty == t.isEmpty) :| "find all" &&
    t.find(_ => false).isEmpty :| "find none" &&
    t.forall(e => t.find(_ == e) == Some(e)) :| "find any elements"
  }

  property("flatMap") = forAll { t: CC[Uniq] =>
    val ints = t.flatMap(_ => create(util.Random.nextInt))
    t.flatMap(_ => create()).isEmpty :| "flatMap to empty" &&
    sameElements(t, t.flatMap(create(_))) :| "flatMap to sameElements" &&
    sameElements(ints, ints.flatMap(i => create(i.toString)).flatMap(s => create(s.toInt))) :| "inverse operations result in equal collection"
  }

  property("flatten") = forAll { t: CC[CC[Uniq]] =>
    sameElements(t, t.map(create(_)).flatten) :| "nest and flatten" &&
    !t.exists(_.isInstanceOf[Uniq]) :| "not flattened" &&
    t.flatten.forall(_.isInstanceOf[Uniq]) :| "flattened"
  }
 
  property("forall") = forAll { t: CC[Uniq] =>
    t.forall(_ => true) :| "forall true" &&
    t.forall(_.isInstanceOf[Uniq]) :| "forall Uniq" &&
    (t.forall(_ => false) == t.isEmpty) :| "forall false" &&
    (t.forall(_.isInstanceOf[UniqSub]) == t.isEmpty) :| "forall UniqSub"
  }

  property("map") = forAll { t: CC[Uniq] => 
    val ints = t.map(_ => util.Random.nextInt)
    sameElements(t, t.map(identity)) :| "map to sameElements" &&
    sameElements(ints, ints.map(_.toString).map(_.toInt)) :| "inverse operations result in equal collection"
  }
}