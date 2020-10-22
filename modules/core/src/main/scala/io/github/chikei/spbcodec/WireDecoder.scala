package io.github.chikei.spbcodec

import com.google.protobuf.{ ByteString, CodedInputStream, WireFormat }
import io.github.chikei.spbcodec.Decoder.Result
import io.github.chikei.spbcodec.error.{ DecodingFailure, InternalProtobufError, WireFieldError }
import io.github.chikei.spbcodec.tag.{
  @@,
  fixed,
  signed,
  signedFixed,
  unsigned,
  Fixed,
  SFixed,
  Signed,
  Unsigned
}

import scala.collection.mutable

trait WireDecoder {
  def tryDecodeTag(
    _wireType: Int,
    input: CodedInputStream,
    ignoreDefault: Boolean
  ): Option[Result[Unit]]
}

object WireDecoder {
  class NumericWireDecoder[A](implicit companion: NumericWireDecoderCompanion[A])
      extends WireDecoder {
    var value: A = companion.defaultValue

    final def tryDecodeTag(
      _wireType: Int,
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Option[Result[Unit]] =
      Option.when(companion.wireType == _wireType) {
        try {
          val v = companion.readValue(input)
          if (ignoreDefault || v != companion.defaultValue) value = v
          Right(())
        } catch { case t: Throwable => Left(InternalProtobufError(t.getMessage, t)) }
      }
  }

  class RepeatedNumericWireDecoder[A](implicit numericDecoder: NumericWireDecoderCompanion[A])
      extends WireDecoder {
    val value = mutable.Buffer.empty[A]

    def tryDecodeTag(
      _wireType: Int,
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Option[Result[Unit]] =
      if (_wireType == WireFormat.WIRETYPE_LENGTH_DELIMITED) {
        try {
          val length   = input.readRawVarint32()
          val oldLimit = input.pushLimit(length)
          while (input.getBytesUntilLimit > 0)
            value.append(readValue(input))
          input.popLimit(oldLimit)
          Some(Right(()))
        } catch {
          case t: Throwable =>
            Some(Left(DecodingFailure.fromThrowable(t, "Errors when reading packed message")))
        }
      } else if (_wireType == wireType) {
        try {
          value.append(readValue(input))
          Some(Right(()))
        } catch {
          case t: Throwable =>
            Some(Left(DecodingFailure.fromThrowable(t, "Errors when reading packed message")))
        }
      } else None

    protected def readValue(input: CodedInputStream): A = numericDecoder.readValue(input)
    protected def wireType: Int                         = numericDecoder.wireType
  }

  sealed trait NumericWireDecoderCompanion[A] {
    def wireType: Int
    def readValue(input: CodedInputStream): A
    def defaultValue: A
  }

  implicit object IntDecoder extends NumericWireDecoderCompanion[Int] {
    def wireType: Int                           = WireFormat.WIRETYPE_VARINT
    def defaultValue: Int                       = 0
    def readValue(input: CodedInputStream): Int = input.readInt32()
  }

  implicit object UnsignedIntDecoder extends NumericWireDecoderCompanion[Int @@ Unsigned] {
    def wireType: Int                                       = WireFormat.WIRETYPE_VARINT
    def defaultValue: Int @@ Unsigned                       = unsigned(0)
    def readValue(input: CodedInputStream): Int @@ Unsigned = unsigned(input.readUInt32())
  }
  implicit object SignedIntDecoder extends NumericWireDecoderCompanion[Int @@ Signed] {
    def wireType: Int                                     = WireFormat.WIRETYPE_VARINT
    def defaultValue: Int @@ Signed                       = signed(0)
    def readValue(input: CodedInputStream): Int @@ Signed = signed(input.readSInt32())
  }
  implicit object FixedIntDecoder extends NumericWireDecoderCompanion[Int @@ Fixed] {
    def wireType: Int                                    = WireFormat.WIRETYPE_FIXED32
    def defaultValue: Int @@ Fixed                       = fixed(0)
    def readValue(input: CodedInputStream): Int @@ Fixed = fixed(input.readFixed32())
  }
  implicit object SignedFixedIntDecoder extends NumericWireDecoderCompanion[Int @@ SFixed] {
    def wireType: Int                                     = WireFormat.WIRETYPE_FIXED32
    def defaultValue: Int @@ SFixed                       = signedFixed(0)
    def readValue(input: CodedInputStream): Int @@ SFixed = signedFixed(input.readSFixed32())
  }
  implicit object LongDecoder extends NumericWireDecoderCompanion[Long] {
    def wireType: Int                            = WireFormat.WIRETYPE_VARINT
    def defaultValue: Long                       = 0
    def readValue(input: CodedInputStream): Long = input.readInt64()
  }
  implicit object UnsignedLongDecoder extends NumericWireDecoderCompanion[Long @@ Unsigned] {
    def wireType: Int                                        = WireFormat.WIRETYPE_VARINT
    def defaultValue: Long @@ Unsigned                       = unsigned(0)
    def readValue(input: CodedInputStream): Long @@ Unsigned = unsigned(input.readUInt64())
  }

  implicit object SignedLongDecoder extends NumericWireDecoderCompanion[Long @@ Signed] {
    def wireType: Int                                      = WireFormat.WIRETYPE_VARINT
    def defaultValue: Long @@ Signed                       = signed(0)
    def readValue(input: CodedInputStream): Long @@ Signed = signed(input.readSInt64())
  }

  implicit object FixedLongDecoder extends NumericWireDecoderCompanion[Long @@ Fixed] {
    def wireType: Int                                     = WireFormat.WIRETYPE_FIXED64
    def defaultValue: Long @@ Fixed                       = fixed(0)
    def readValue(input: CodedInputStream): Long @@ Fixed = fixed(input.readFixed64())
  }

  implicit object SignedFixedLongDecoder extends NumericWireDecoderCompanion[Long @@ SFixed] {
    def wireType: Int                                      = WireFormat.WIRETYPE_FIXED64
    def defaultValue: Long @@ SFixed                       = signedFixed(0)
    def readValue(input: CodedInputStream): Long @@ SFixed = signedFixed(input.readSFixed64())
  }

  implicit object BooleanDecoder extends NumericWireDecoderCompanion[Boolean] {
    def wireType: Int                               = WireFormat.WIRETYPE_VARINT
    def defaultValue: Boolean                       = false
    def readValue(input: CodedInputStream): Boolean = input.readBool()
  }

  implicit object FloatDecoder extends NumericWireDecoderCompanion[Float] {
    def wireType: Int                             = WireFormat.WIRETYPE_FIXED32
    def defaultValue: Float                       = 0f
    def readValue(input: CodedInputStream): Float = input.readFloat()
  }

  implicit object DoubleDecoder extends NumericWireDecoderCompanion[Double] {
    def wireType: Int                              = WireFormat.WIRETYPE_FIXED64
    def defaultValue: Double                       = 0d
    def readValue(input: CodedInputStream): Double = input.readDouble()
  }

  object EnumDecoder extends NumericWireDecoderCompanion[Int] {
    def wireType: Int                           = WireFormat.WIRETYPE_VARINT
    def defaultValue: Int                       = 0
    def readValue(input: CodedInputStream): Int = input.readEnum()
  }

  final class IntDecoder                     extends NumericWireDecoder[Int]
  final class RepeatedIntDecoder             extends RepeatedNumericWireDecoder[Int]
  final class UnsignedIntDecoder             extends NumericWireDecoder[Int @@ Unsigned]
  final class RepeatedUnsignedIntDecoder     extends RepeatedNumericWireDecoder[Int @@ Unsigned]
  final class SignedIntDecoder               extends NumericWireDecoder[Int @@ Signed]
  final class RepeatedSignedIntDecoder       extends RepeatedNumericWireDecoder[Int @@ Signed]
  final class FixedIntDecoder                extends NumericWireDecoder[Int @@ Fixed]
  final class RepeatedFixedIntDecoder        extends RepeatedNumericWireDecoder[Int @@ Fixed]
  final class SignedFixedIntDecoder          extends NumericWireDecoder[Int @@ SFixed]
  final class RepeatedSignedFixedIntDecoder  extends RepeatedNumericWireDecoder[Int @@ SFixed]
  final class LongDecoder                    extends NumericWireDecoder[Long]
  final class RepeatedLongDecoder            extends RepeatedNumericWireDecoder[Long]
  final class UnsignedLongDecoder            extends NumericWireDecoder[Long @@ Unsigned]
  final class RepeatedUnsignedLongDecoder    extends RepeatedNumericWireDecoder[Long @@ Unsigned]
  final class SignedLongDecoder              extends NumericWireDecoder[Long @@ Signed]
  final class RepeatedSignedLongDecoder      extends RepeatedNumericWireDecoder[Long @@ Signed]
  final class FixedLongDecoder               extends NumericWireDecoder[Long @@ Fixed]
  final class RepeatedFixedLongDecoder       extends RepeatedNumericWireDecoder[Long @@ Fixed]
  final class SignedFixedLongDecoder         extends NumericWireDecoder[Long @@ SFixed]
  final class RepeatedSignedFixedLongDecoder extends RepeatedNumericWireDecoder[Long @@ SFixed]
  final class BooleanDecoder                 extends NumericWireDecoder[Boolean]
  final class RepeatedBooleanDecoder         extends RepeatedNumericWireDecoder[Boolean]
  final class FloatDecoder                   extends NumericWireDecoder[Float]
  final class RepeatedFloatDecoder           extends RepeatedNumericWireDecoder[Float]
  final class DoubleDecoder                  extends NumericWireDecoder[Double]
  final class RepeatedDoubleDecoder          extends RepeatedNumericWireDecoder[Double]
  final class EnumDecoder                    extends NumericWireDecoder[Int]()(EnumDecoder)
  final class RepeatedEnumDecoder            extends RepeatedNumericWireDecoder[Int]()(EnumDecoder)

  abstract class LengthDelimitedWireDecoder extends WireDecoder {
    def tryDecodeTag(
      _wireType: Int,
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Option[Result[Unit]] =
      Option.when(_wireType == WireFormat.WIRETYPE_LENGTH_DELIMITED) {
        tryDecode(input: CodedInputStream, ignoreDefault: Boolean)
      }

    protected[spbcodec] def tryDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit]
  }

  class StringDecoder extends LengthDelimitedWireDecoder {
    var value = ""
    protected[spbcodec] def tryDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit] =
      try {
        val v = input.readStringRequireUtf8()
        if (ignoreDefault || v.nonEmpty) value = v
        Right(())
      } catch { case t: Throwable => Left(InternalProtobufError(t.getMessage, t)) }
  }

  class ByteStringDecoder extends LengthDelimitedWireDecoder {
    var value = ByteString.EMPTY
    protected[spbcodec] def tryDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit] =
      try {
        val v = input.readBytes()
        if (ignoreDefault || !v.isEmpty) value = v
        Right(())
      } catch { case t: Throwable => Left(InternalProtobufError(t.getMessage, t)) }
  }

  class RepeatedStringDecoder extends LengthDelimitedWireDecoder {
    val value = mutable.Buffer.empty[String]
    protected[spbcodec] def tryDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit] =
      try {
        val v = input.readStringRequireUtf8()
        value.append(v)
        Right(())
      } catch { case t: Throwable => Left(InternalProtobufError(t.getMessage, t)) }
  }

  class RepeatedByteStringDecoder extends LengthDelimitedWireDecoder {
    val value = mutable.Buffer.empty[ByteString]
    protected[spbcodec] def tryDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit] =
      try {
        val v = input.readBytes()
        value.append(v)
        Right(())
      } catch { case t: Throwable => Left(InternalProtobufError(t.getMessage, t)) }
  }

  class RepeatedMessageDecoder[A](val fieldDecoder: MessageFieldDecoder[A])
      extends LengthDelimitedWireDecoder {
    val value = mutable.Buffer.empty[A]
    protected[spbcodec] def tryDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit] = {
      val msgWireDecoder = fieldDecoder.messageWireDecoder
      val elem = msgWireDecoder
        .tryDecode(input, ignoreDefault)
        .flatMap(_ => fieldDecoder.build(msgWireDecoder))
      elem.foreach(value.append)
      elem.map(_ => ())
    }
  }

  abstract class DelimitedMessageWireDecoder extends LengthDelimitedWireDecoder {
    protected[spbcodec] def tryDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit] =
      try {
        val length   = input.readRawVarint32()
        val oldLimit = input.pushLimit(length)
        val r        = doDecode(input, ignoreDefault)
        input.checkLastTagWas(0)
        input.popLimit(oldLimit)
        r
      } catch { case t: Throwable => Left(InternalProtobufError(t.getMessage, t)) }

    protected[spbcodec] def doDecode(input: CodedInputStream, ignoreDefault: Boolean): Result[Unit]
  }

  class MessageDecoder(decoders: Map[Int, WireDecoder]) extends DelimitedMessageWireDecoder {
    private var _seen: Set[Int] = Set.empty

    def value: Map[Int, WireDecoder] = decoders
    def seen: Set[Int]               = _seen

    protected[spbcodec] def doDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit] = {
      var done                     = false
      var failure: DecodingFailure = null
      while (!done && failure == null)
        try {
          val tag = input.readTag()
          if (tag == 0) done = true
          else {
            val wireType = WireFormat.getTagWireType(tag)
            val index    = WireFormat.getTagFieldNumber(tag)

            decoders.get(index).foreach { decoder =>
              decoder.tryDecodeTag(wireType, input, ignoreDefault) match {
                case Some(Left(t))  => failure = WireFieldError(index, t)
                case Some(Right(_)) => _seen = _seen.incl(index)
                case None =>
                  try input.skipField(tag)
                  catch {
                    case e: Throwable =>
                      failure =
                        DecodingFailure.fromThrowable(e, s"Errors when skip field with tag $tag")
                  }
              }
            }
          }
        } catch {
          case e: Throwable =>
            failure = DecodingFailure.fromThrowable(e, s"Errors when reading tag")
        }
      if (failure != null) Left(failure)
      else Right(())
    }
  }

  class OneofDecoder(decoders: Map[Int, () => WireDecoder]) extends DelimitedMessageWireDecoder {
    def lastDecoder: Option[WireDecoder] = _lastDecoder
    def lastOneof: Int                   = _lastOneof

    private var _lastOneof: Int                   = 0
    private var _lastDecoder: Option[WireDecoder] = None

    def decode(input: CodedInputStream, ignoreDefault: Boolean = false): Result[Unit] =
      doDecode(input, ignoreDefault)

    protected[spbcodec] def doDecode(
      input: CodedInputStream,
      ignoreDefault: Boolean
    ): Result[Unit] = {
      var done                     = false
      var failure: DecodingFailure = null
      while (!done && failure == null)
        try {
          val tag = input.readTag()
          if (tag == 0) done = true
          else {
            val wireType = WireFormat.getTagWireType(tag)
            val index    = WireFormat.getTagFieldNumber(tag)

            decoders.get(index).foreach { makeDecoder =>
              val decoder = _lastDecoder match {
                case Some(decoder) if _lastOneof == index => decoder
                case _                                    => makeDecoder()
              }
              decoder.tryDecodeTag(wireType, input, ignoreDefault) match {
                case Some(Left(t)) => failure = WireFieldError(index, t)
                case Some(Right(_)) =>
                  _lastOneof = index
                  _lastDecoder = Option(decoder)
                case None =>
                  try input.skipField(tag)
                  catch {
                    case e: Throwable =>
                      failure =
                        DecodingFailure.fromThrowable(e, s"Errors when skip field with tag $tag")
                  }
              }
            }
          }
        } catch {
          case e: Throwable =>
            failure = DecodingFailure.fromThrowable(e, s"Errors when reading tag")
        }
      if (failure != null) Left(failure)
      else Right(())
    }
  }
}
