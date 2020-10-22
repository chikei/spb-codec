package io.github.chikei.spbcodec.magnolia.decoding

import io.github.chikei.spbcodec.magnolia.Index
import io.github.chikei.spbcodec.{ Decoder, FieldDecoder }
import magnolia.{ CaseClass, Magnolia, SealedTrait }

object semiauto {
  type Typeclass[T] = FieldDecoder[T]
  def combine[T](caseClass: CaseClass[FieldDecoder, T])(implicit index: Index[T]): Decoder[T] =
    MagnoliaDecoder.combine(caseClass)
  def dispatch[T](sealedTrait: SealedTrait[FieldDecoder, T])(implicit index: Index[T]): Decoder[T] =
    MagnoliaDecoder.dispatch(sealedTrait)
  def deriveDecoder[T]: Decoder[T] = macro Magnolia.gen[T]
}
