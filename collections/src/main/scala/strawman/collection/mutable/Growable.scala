package strawman
package collection
package mutable

import strawman.collection.IterableOnce
import scala.{`inline`, Unit, deprecated}
import scala.annotation.tailrec

/** This trait forms part of collections that can be augmented
 *  using a `+=` operator and that can be cleared of all elements using
 *  a `clear` method.
 */
trait Growable[-A] extends Clearable {

  /** ${Add}s a single element to this $coll.
   *
   *  @param elem  the element to $add.
   *  @return the $coll itself
   */
  def add(elem: A): this.type

  /** Alias for `add` */
  @`inline` final def += (elem: A): this.type = add(elem)

  //TODO This causes a conflict in StringBuilder; looks like a compiler bug
  //@deprecated("Use add or += instead of append", "2.13.0")
  //@`inline` final def append(elem: A): Unit = add(elem)

  /** ${Add}s two or more elements to this $coll.
   *
   *  @param elem1 the first element to $add.
   *  @param elem2 the second element to $add.
   *  @param elems the remaining elements to $add.
   *  @return the $coll itself
   */
  @`inline` final def += (elem1: A, elem2: A, elems: A*): this.type = this += elem1 += elem2 ++= (elems.toStrawman: IterableOnce[A])

  /** ${Add}s all elements produced by a TraversableOnce to this $coll.
   *
   *  @param xs   the TraversableOnce producing the elements to $add.
   *  @return  the $coll itself.
   */
  def addAll(xs: IterableOnce[A]): this.type = {
    @tailrec def loop(xs: collection.LinearSeq[A]): Unit = {
      if (xs.nonEmpty) {
        this += xs.head
        loop(xs.tail)
      }
    }
    xs match {
      case xs: collection.LinearSeq[A] => loop(xs)
      case xs => xs.iterator() foreach += // Deviation: IterableOnce does not define `foreach`.
    }
    // @ichoran writes: Right now, this actually isn't any faster than using an iterator
    // for List. Maybe we should just simplify the code by deferring to iterator foreach?
    this
  }

  /** Alias for `addAllInPlace` */
  @`inline` final def ++= (xs: IterableOnce[A]): this.type = addAll(xs)
}

object Growable {

  /**
    * Fills a `Growable` instance with the elements of a given iterable
    * @param empty Instance to fill
    * @param it Elements to add
    * @tparam A Element type
    * @return The filled instance
    */
  def from[A](empty: Growable[A], it: collection.IterableOnce[A]): empty.type = empty ++= it

}

/** This trait forms part of collections that can be cleared
  *  with a clear() call.
  */
trait Clearable {
  /** Clears the $coll's contents. After this operation, the
    *  $coll is empty.
    */
  def clear(): Unit
}
