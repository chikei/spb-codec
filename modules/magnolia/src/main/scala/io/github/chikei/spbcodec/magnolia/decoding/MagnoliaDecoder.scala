package io.github.chikei.spbcodec.magnolia.decoding

import io.github.chikei.spbcodec.Decoder.Result
import io.github.chikei.spbcodec.WireDecoder.{ MessageDecoder, OneofDecoder }
import io.github.chikei.spbcodec.error.{
  MissingFieldDecoder,
  OneofNotSet,
  UnknownSubtypeIndex,
  WrongWireDecoderType
}
import io.github.chikei.spbcodec.magnolia.Index
import io.github.chikei.spbcodec.{ Decoder, FieldDecoder, WireDecoder }
import magnolia.{ CaseClass, SealedTrait }

private[magnolia] object MagnoliaDecoder {
  private[magnolia] def combine[T](
    caseClass: CaseClass[FieldDecoder, T]
  )(implicit index: Index[T]): Decoder[T] =
    new Decoder[T] {
      def build(wireDecoder: WireDecoder): Result[T] =
        wireDecoder match {
          case msg: MessageDecoder =>
            val decoders = msg.value
            caseClass.constructMonadic { p =>
              decoders
                .get(index.index(p.index))
                .toRight(MissingFieldDecoder(index.index(p.index), p.label))
                .flatMap(wireDecoder => p.typeclass.build(wireDecoder))
            }
          case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
        }

      def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder = {
        val decoders =
          caseClass.parameters.map(p => (index.index(p.index), p.typeclass.wireDecoder)).toMap
        new MessageDecoder(decoders)
      }
    }

  private[magnolia] def dispatch[T](
    sealedTrait: SealedTrait[FieldDecoder, T]
  )(implicit index: Index[T]): Decoder[T] =
    new Decoder[T] {
      def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder = {
        val decoders =
          sealedTrait.subtypes.map(p => (index.index(p.index), () => p.typeclass.wireDecoder)).toMap
        new OneofDecoder(decoders)
      }

      def build(wireDecoder: WireDecoder): Result[T] =
        wireDecoder match {
          case oneof: OneofDecoder =>
            if (oneof.lastOneof == 0) Left(OneofNotSet(sealedTrait.typeName.short))
            else {
              oneof.lastDecoder.zip(
                sealedTrait.subtypes.find(p => index.index(p.index) == oneof.lastOneof)
              ) match {
                case Some((wireDecoder, sub)) => sub.typeclass.build(wireDecoder)
                case None                     => Left(UnknownSubtypeIndex(oneof.lastOneof, sealedTrait.typeName.short))
              }
            }
          case _ => Left(WrongWireDecoderType("OneofDecoder", wireDecoder))
        }
    }
}
