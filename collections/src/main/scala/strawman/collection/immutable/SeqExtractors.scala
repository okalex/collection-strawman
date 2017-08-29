package strawman.collection
package immutable

import scala.{Option, Some, None}

/** An extractor used to head/tail deconstruct sequences. */
object +: {
  def unapply[T, CC[_], Coll <: SeqOps[T, CC, Coll]](
      t: Coll with SeqOps[T, CC, Coll]): Option[(T, Coll)] =
    if(t.isEmpty) None
    else Some((t.head, t.tail))
}

/** An extractor used to init/last deconstruct sequences. */
object :+ {
  /** Splits a sequence into init :+ last.
   * @return Some((init, last)) if sequence is non-empty. None otherwise.
   */
  def unapply[T, CC[_], Coll <: SeqOps[T, CC, Coll]](
      t: Coll with SeqOps[T, CC, Coll]): Option[(Coll, T)] =
    if(t.isEmpty) None
    else Some((t.init, t.last))
}
