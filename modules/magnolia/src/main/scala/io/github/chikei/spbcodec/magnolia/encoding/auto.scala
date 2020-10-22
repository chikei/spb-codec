package io.github.chikei.spbcodec.magnolia.encoding

import io.github.chikei.spbcodec.magnolia.Index
import io.github.chikei.spbcodec.{ Encoder, FieldEncoder }
import magnolia.{ CaseClass, Magnolia, SealedTrait }

object auto {
  type Typeclass[T] = FieldEncoder[T]
  def combine[T](caseClass: CaseClass[Typeclass, T])(implicit index: Index[T]): Encoder[T] =
    MagnoliaEncoder.combine(caseClass)
  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T])(implicit index: Index[T]): Encoder[T] =
    MagnoliaEncoder.dispatch(sealedTrait)
  implicit def magnoliaEncoder[T]: Encoder[T] = macro Magnolia.gen[T]
}
