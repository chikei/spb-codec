package io.github.chikei.spbcodec.magnolia.encoding

import com.google.protobuf.CodedOutputStream
import io.github.chikei.spbcodec.Encoder.MessageEncoder
import io.github.chikei.spbcodec.magnolia.Index
import io.github.chikei.spbcodec.{ Encoder, FieldEncoder }
import magnolia.{ CaseClass, SealedTrait }

private[magnolia] object MagnoliaEncoder {
  private[magnolia] def combine[T](
    caseClass: CaseClass[FieldEncoder, T]
  )(implicit index: Index[T]): Encoder[T] =
    new MessageEncoder[T] {
      def isDefault(a: T): Boolean =
        if (caseClass.isObject) true
        else caseClass.parameters.forall(param => param.typeclass.isDefault(param.dereference(a)))

      def writeTo(a: T, output: CodedOutputStream): Unit =
        if (caseClass.isObject) ()
        else {
          caseClass.parameters.foreach { param =>
            val tc    = param.typeclass
            val field = param.dereference(a)
            if (!tc.isDefault(field)) tc.writeToTag(index.index(param.index), field, output)
          }
        }

      def size(a: T): Int =
        if (caseClass.isObject) 0
        else {
          caseClass.parameters.foldLeft(0) { case (sz, param) =>
            val tc    = param.typeclass
            val field = param.dereference(a)
            val fieldSize =
              if (tc.isDefault(field)) 0 else tc.sizeTag(index.index(param.index), field)
            sz + fieldSize
          }
        }
    }

  private[magnolia] def dispatch[T](
    sealedTrait: SealedTrait[FieldEncoder, T]
  )(implicit index: Index[T]): Encoder[T] =
    new MessageEncoder[T] {
      def isDefault(a: T): Boolean = false

      def writeTo(a: T, output: CodedOutputStream): Unit =
        sealedTrait.dispatch(a) { subtype =>
          val oneOfFieldNumber = index.index(subtype.index)
          subtype.typeclass.writeToTag(oneOfFieldNumber, subtype.cast(a), output)
        }

      def size(a: T): Int =
        sealedTrait.dispatch(a) { subtype =>
          val oneOfFieldNumber = index.index(subtype.index)
          subtype.typeclass.sizeTag(oneOfFieldNumber, subtype.cast(a))
        }
    }
}
