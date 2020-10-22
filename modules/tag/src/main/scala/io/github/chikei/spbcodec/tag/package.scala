package io.github.chikei.spbcodec

import io.github.chikei.spbcodec.tag.TagRestriction.NumericTagRestriction

/**
 * Use tags to refine int/long values as [[tag.Signed]], [[tag.Unsigned]] or [[tag.Fixed]].
 */
package object tag {

  /**
   * copied from https://raw.githubusercontent.com/softwaremill/scala-common/master/tagging/src/main/scala/com/softwaremill/tagging/package.scala
   */
  type @@[+T, +U] = T with Tag[U]
  implicit class Tagger[T](val t: T) extends AnyVal {
    def taggedWith[U]: T @@ U = t.asInstanceOf[T @@ U]
  }
  implicit class AndTagger[T, U](val t: T @@ U) extends AnyVal {
    def andTaggedWith[V]: T @@ (U with V) = t.asInstanceOf[T @@ (U with V)]
  }

  /**
   * Tag a numeric field with a `Unsigned` tag
   */
  def unsigned[T: NumericTagRestriction](t: T): T @@ Unsigned = t.taggedWith[Unsigned]

  /**
   * Tag a numeric field with a `Signed` tag
   */
  def signed[T: NumericTagRestriction](t: T): T @@ Signed = t.taggedWith[Signed]

  /**
   * Tag a numeric field with a `Fixed` tag
   */
  def fixed[T: NumericTagRestriction](t: T): T @@ Fixed = t.taggedWith[Fixed]

  /**
   * Tag a numeric field with a `Signed` and `Fixed` tag
   */
  def signedFixed[T: NumericTagRestriction](t: T): T @@ Signed with Fixed =
    t.taggedWith[Signed].andTaggedWith[Fixed]

  type SFixed = Signed with Fixed
}
