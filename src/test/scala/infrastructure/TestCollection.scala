package infrastructure

import scala.collection.GenTraversableOnce
import org.scalacheck.util.Buildable

trait TestCollection {

  /** The collection type */
  type CC[_]
  
  /** Tell scalacheck how to build collections of this type */
  implicit def buildableCC[T]: Buildable[T, CC]

  /** Create a collection of this type with the specified elements */
  def create[T](ts: T*): CC[T]
  
  /** Is this collection an ordered collection? */
  val orderedCollection: Boolean
  
  /** Do the two collections have the same elements? For ordered collections
   *  they need to be in the same order, too. */
  def sameElements[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean
  
  /** Does `container` contain all elements of `contained`? For ordered
   *  collections, they need to be in the same order, too. */
  def containsSlice[T](container: GenTraversableOnce[T], contained: GenTraversableOnce[T]): Boolean
}


trait TestCollectionOrdered extends TestCollection {
  
  val orderedCollection: Boolean = true
  
  def sameElements[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean =
    sameElementsOrdered(a, b)
  
  def containsSlice[T](container: GenTraversableOnce[T], contained: GenTraversableOnce[T]): Boolean =
    containsAllInOrder(container, contained)
}


trait TestCollectionUnordered extends TestCollection {
  
  val orderedCollection: Boolean = false
  
  def sameElements[T](a: GenTraversableOnce[T], b: GenTraversableOnce[T]): Boolean =
    sameElementsUnordered(a, b)
  
  def containsSlice[T](container: GenTraversableOnce[T], contained: GenTraversableOnce[T]): Boolean =
    containsAll(container, contained)
}