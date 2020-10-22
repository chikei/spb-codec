package io.github.chikei.spbcodec

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.util.UUID

import com.google.protobuf.{ ByteString, CodedOutputStream, WireFormat }
import io.github.chikei.spbcodec.Encoder.MessageEncoder
import io.github.chikei.spbcodec.tag.{ @@, signed, Fixed, SFixed, Signed, Unsigned }

import scala.annotation.implicitNotFound

/**
 * Interface for Encoder implementations.
 *
 * Allows to encode instances of `A` as protobuf3 serialized message.
 */
@implicitNotFound("No Encoder found for type ${A}.")
trait Encoder[A] extends FieldEncoder[A] { self =>

  def writeTo(a: A, output: CodedOutputStream): Unit

  def size(a: A): Int

  /**
   * Encode the value A into an ByteArrayOutputStream.
   */
  def encodeAsStream(a: A): ByteArrayOutputStream = {
    val out = new ByteArrayOutputStream()
    val cos = CodedOutputStream.newInstance(out)
    writeTo(a, cos)
    cos.flush()
    out
  }

  /**
   * Encode the value A and return the result in Array[Byte].
   */
  def encodeAsBytes(a: A): Array[Byte] =
    encodeAsStream(a).toByteArray

  /**
   * Encode the value A and return the result as a ByteBuffer.
   */
  def encodeAsByteBuffer(a: A): ByteBuffer =
    ByteBuffer.wrap(encodeAsBytes(a))

  /**
   * Create a new [[Encoder]] by applying a function to a value of type `B` before writing as an A.
   */
  final def contramap[B](f: B => A): Encoder[B] = self match {
    case encoder: Encoder.MappedEncoder[A, _] =>
      new Encoder.MappedEncoder(f.andThen(encoder.f), encoder.encoder)
    case _ => new Encoder.MappedEncoder(f, self)
  }
}

/**
 * Interface for FieldEncoder implementations.
 *
 * Allows to encode instances of `A` as a field of a protobuf message.
 *
 * We need this because non-packed repeated message always need field number to encoding as
 * they are encoded as multiple [fieldNumber:length-delimited message] pair with same field number.
 */
@implicitNotFound("No FieldEncoder found for type ${A}.")
trait FieldEncoder[A] extends Serializable { self =>

  def isDefault(a: A): Boolean

  def writeToTag(fieldNumber: Int, a: A, output: CodedOutputStream): Unit

  def sizeTag(fieldNumber: Int, a: A): Int

  def writeIterableToTag(fieldNumber: Int, as: Iterable[A], output: CodedOutputStream): Unit =
    as.foreach(writeToTag(fieldNumber, _, output))

  def sizeIterableTag(fieldNumber: Int, as: Iterable[A]): Int =
    as.foldLeft(0) { case (sz, a) =>
      sz + this.sizeTag(fieldNumber, a)
    }

  final def contramapField[B](f: B => A): FieldEncoder[B] = self match {
    case encoder: FieldEncoder.MappedFieldEncoder[A, _] =>
      new FieldEncoder.MappedFieldEncoder(f.andThen(encoder.f), encoder.encoder)
    case _ => new FieldEncoder.MappedFieldEncoder(f, self)
  }
}

/**
 * Utilities for [[Encoder]].
 */
object Encoder {

  /**
   * Return an instance for a given type `A`.
   */
  final def apply[A](implicit instance: Encoder[A]): Encoder[A] = instance

  abstract class MessageEncoder[T] extends Encoder[T] {
    def writeToTag(fieldNumber: Int, a: T, output: CodedOutputStream): Unit = {
      output.writeTag(fieldNumber, WireFormat.WIRETYPE_LENGTH_DELIMITED)
      output.writeUInt32NoTag(this.size(a))
      writeTo(a, output)
    }

    def sizeTag(fieldNumber: Int, a: T): Int = {
      val _size = size(a)
      _size +
        CodedOutputStream.computeTagSize(fieldNumber) +
        CodedOutputStream.computeUInt32SizeNoTag(_size)
    }
  }

  private class MappedEncoder[F, T](val f: F => T, val encoder: Encoder[T]) extends Encoder[F] {
    def writeTo(a: F, output: CodedOutputStream): Unit = encoder.writeTo(f(a), output)
    def size(a: F): Int                                = encoder.size(f(a))
    def isDefault(a: F): Boolean                       = encoder.isDefault(f(a))
    def writeToTag(fieldNumber: Int, a: F, output: CodedOutputStream): Unit =
      encoder.writeToTag(fieldNumber, f(a), output)
    def sizeTag(fieldNumber: Int, a: F): Int = encoder.sizeTag(fieldNumber, f(a))
    override def writeIterableToTag(
      fieldNumber: Int,
      as: Iterable[F],
      output: CodedOutputStream
    ): Unit = encoder.writeIterableToTag(fieldNumber, as.map(f), output)
    override def sizeIterableTag(fieldNumber: Int, as: Iterable[F]): Int =
      encoder.sizeIterableTag(fieldNumber, as.map(f))
  }
}

private[spbcodec] trait MidPriorityEncoders {
  implicit final def encodeIterableEv[A, C[_]](implicit
    encoder: Encoder[A],
    ev: C[A] => Iterable[A]
  ): FieldEncoder[C[A]] = new IterableEncoder[A, C](encoder) {
    final protected def toIterable(a: C[A]): Iterable[A] = ev(a)
  }

  abstract protected[this] class IterableEncoder[A, C[_]](encoder: Encoder[A])
      extends FieldEncoder[C[A]] {
    protected def toIterable(a: C[A]): Iterable[A]
    def isDefault(a: C[A]): Boolean = toIterable(a).isEmpty
    def writeToTag(fieldNumber: Int, a: C[A], output: CodedOutputStream): Unit =
      encoder.writeIterableToTag(fieldNumber, toIterable(a), output)
    def sizeTag(fieldNumber: Int, a: C[A]): Int =
      encoder.sizeIterableTag(fieldNumber, toIterable(a))
  }
}

object FieldEncoder extends Encoders with MidPriorityEncoders {
  final def apply[A](implicit instance: FieldEncoder[A]): FieldEncoder[A] = instance

  private class MappedFieldEncoder[F, T](val f: F => T, val encoder: FieldEncoder[T])
      extends FieldEncoder[F] {
    def isDefault(a: F): Boolean = encoder.isDefault(f(a))
    def writeToTag(fieldNumber: Int, a: F, output: CodedOutputStream): Unit =
      encoder.writeToTag(fieldNumber, f(a), output)
    def sizeTag(fieldNumber: Int, a: F): Int = encoder.sizeTag(fieldNumber, f(a))
    override def writeIterableToTag(
      fieldNumber: Int,
      as: Iterable[F],
      output: CodedOutputStream
    ): Unit = encoder.writeIterableToTag(fieldNumber, as.map(f), output)
    override def sizeIterableTag(fieldNumber: Int, as: Iterable[F]): Int =
      encoder.sizeIterableTag(fieldNumber, as.map(f))
  }

  // collections
  implicit final def encodeSeq[A](implicit encoder: Encoder[A]): FieldEncoder[Seq[A]] =
    new IterableEncoder[A, Seq](encoder) {
      final protected def toIterable(a: Seq[A]): Iterable[A] = a.toIterable
    }

  implicit final val encodeUUID: FieldEncoder[UUID] = encodeSeq[Long @@ Signed]
    .contramapField(uuid =>
      Seq(signed(uuid.getMostSignificantBits), signed(uuid.getLeastSignificantBits))
    )
}

private[spbcodec] trait Encoders {
  // numeric type use packed encoding
  abstract private class NumericEncoder[A] extends Encoder[A] {
    final override def writeIterableToTag(
      fieldNumber: Int,
      as: Iterable[A],
      output: CodedOutputStream
    ): Unit = {
      output.writeTag(fieldNumber, WireFormat.WIRETYPE_LENGTH_DELIMITED)
      output.writeUInt32NoTag(sizeIterable(as))
      as.foreach(a => writeTo(a, output))
    }

    final override def sizeIterableTag(fieldNumber: Int, as: Iterable[A]): Int = {
      val _size = sizeIterable(as)
      _size +
        CodedOutputStream.computeTagSize(fieldNumber) +
        CodedOutputStream.computeUInt32SizeNoTag(_size)
    }

    // packed encoding does not include tag per element
    protected def sizeIterable(as: Iterable[A]): Int = as.foldLeft(0) { case (sz, a) =>
      sz + size(a)
    }
  }

  abstract private class FixedEncoder[A] extends NumericEncoder[A] {
    final override protected def sizeIterable(as: Iterable[A]): Int = as.size * fixedSize
    protected val fixedSize: Int
  }

  // encoders for protobuf primitive types
  implicit final val encodeInt: Encoder[Int] = new NumericEncoder[Int] {
    def isDefault(a: Int): Boolean                       = a == 0
    def writeTo(a: Int, output: CodedOutputStream): Unit = output.writeInt32NoTag(a)
    def writeToTag(fieldNumber: Int, a: Int, output: CodedOutputStream): Unit =
      output.writeInt32(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Int): Int = CodedOutputStream.computeInt32Size(fieldNumber, a)
    def size(a: Int): Int                      = CodedOutputStream.computeInt32SizeNoTag(a)
  }

  implicit final val encodeUnsignedInt: Encoder[Int @@ Unsigned] =
    new NumericEncoder[Int @@ Unsigned] {
      def isDefault(a: Int @@ Unsigned): Boolean                       = a == 0
      def writeTo(a: Int @@ Unsigned, output: CodedOutputStream): Unit = output.writeUInt32NoTag(a)
      def writeToTag(fieldNumber: Int, a: Int @@ Unsigned, output: CodedOutputStream): Unit =
        output.writeUInt32(fieldNumber, a)
      def sizeTag(fieldNumber: Int, a: Int @@ Unsigned): Int =
        CodedOutputStream.computeUInt32Size(fieldNumber, a)
      def size(a: Int @@ Unsigned): Int = CodedOutputStream.computeUInt32SizeNoTag(a)
    }

  implicit final val encodeSignedInt: Encoder[Int @@ Signed] = new NumericEncoder[Int @@ Signed] {
    def isDefault(a: Int @@ Signed): Boolean                       = a == 0
    def writeTo(a: Int @@ Signed, output: CodedOutputStream): Unit = output.writeSInt32NoTag(a)
    def writeToTag(fieldNumber: Int, a: Int @@ Signed, output: CodedOutputStream): Unit =
      output.writeSInt32(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Int @@ Signed): Int =
      CodedOutputStream.computeSInt32Size(fieldNumber, a)
    def size(a: Int @@ Signed): Int = CodedOutputStream.computeSInt32SizeNoTag(a)
  }

  implicit final val encodeFixedInt: Encoder[Int @@ Fixed] = new FixedEncoder[Int @@ Fixed] {
    def isDefault(a: Int @@ Fixed): Boolean                       = a == 0
    def writeTo(a: Int @@ Fixed, output: CodedOutputStream): Unit = output.writeFixed32NoTag(a)
    def writeToTag(fieldNumber: Int, a: Int @@ Fixed, output: CodedOutputStream): Unit =
      output.writeFixed32(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Int @@ Fixed): Int =
      CodedOutputStream.computeFixed32Size(fieldNumber, a)
    def size(a: Int @@ Fixed): Int     = CodedOutputStream.computeFixed32SizeNoTag(a)
    final protected val fixedSize: Int = 4
  }

  implicit final val encodeSignedFixedInt: Encoder[Int @@ SFixed] =
    new FixedEncoder[Int @@ SFixed] {
      def isDefault(a: Int @@ SFixed): Boolean                       = a == 0
      def writeTo(a: Int @@ SFixed, output: CodedOutputStream): Unit = output.writeSFixed32NoTag(a)
      def writeToTag(fieldNumber: Int, a: Int @@ SFixed, output: CodedOutputStream): Unit =
        output.writeSFixed32(fieldNumber, a)
      def sizeTag(fieldNumber: Int, a: Int @@ SFixed): Int =
        CodedOutputStream.computeSFixed32Size(fieldNumber, a)
      def size(a: Int @@ SFixed): Int    = CodedOutputStream.computeSFixed32SizeNoTag(a)
      final protected val fixedSize: Int = 4
    }

  implicit final val encodeLong: Encoder[Long] = new NumericEncoder[Long] {
    def isDefault(a: Long): Boolean                       = a == 0L
    def writeTo(a: Long, output: CodedOutputStream): Unit = output.writeInt64NoTag(a)
    def writeToTag(fieldNumber: Int, a: Long, output: CodedOutputStream): Unit =
      output.writeInt64(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Long): Int = CodedOutputStream.computeInt64Size(fieldNumber, a)
    def size(a: Long): Int                      = CodedOutputStream.computeInt64SizeNoTag(a)
  }

  implicit final val encodeUnsignedLong: Encoder[Long @@ Unsigned] =
    new NumericEncoder[Long @@ Unsigned] {
      def isDefault(a: Long @@ Unsigned): Boolean                       = a == 0L
      def writeTo(a: Long @@ Unsigned, output: CodedOutputStream): Unit = output.writeUInt64NoTag(a)
      def writeToTag(fieldNumber: Int, a: Long @@ Unsigned, output: CodedOutputStream): Unit =
        output.writeUInt64(fieldNumber, a)
      def sizeTag(fieldNumber: Int, a: Long @@ Unsigned): Int =
        CodedOutputStream.computeUInt64Size(fieldNumber, a)
      def size(a: Long @@ Unsigned): Int = CodedOutputStream.computeUInt64SizeNoTag(a)
    }

  implicit final val encodeSignedLong: Encoder[Long @@ Signed] =
    new NumericEncoder[Long @@ Signed] {
      def isDefault(a: Long @@ Signed): Boolean                       = a == 0L
      def writeTo(a: Long @@ Signed, output: CodedOutputStream): Unit = output.writeSInt64NoTag(a)
      def writeToTag(fieldNumber: Int, a: Long @@ Signed, output: CodedOutputStream): Unit =
        output.writeSInt64(fieldNumber, a)
      def sizeTag(fieldNumber: Int, a: Long @@ Signed): Int =
        CodedOutputStream.computeSInt64Size(fieldNumber, a)
      def size(a: Long @@ Signed): Int = CodedOutputStream.computeSInt64SizeNoTag(a)
    }

  implicit final val encodeFixedLong: Encoder[Long @@ Fixed] = new FixedEncoder[Long @@ Fixed] {
    def isDefault(a: Long @@ Fixed): Boolean                       = a == 0L
    def writeTo(a: Long @@ Fixed, output: CodedOutputStream): Unit = output.writeFixed64NoTag(a)
    def writeToTag(fieldNumber: Int, a: Long @@ Fixed, output: CodedOutputStream): Unit =
      output.writeFixed64(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Long @@ Fixed): Int =
      CodedOutputStream.computeFixed64Size(fieldNumber, a)
    def size(a: Long @@ Fixed): Int    = CodedOutputStream.computeFixed64SizeNoTag(a)
    final protected val fixedSize: Int = 8
  }

  implicit final val encodeSignedFixedLong: Encoder[Long @@ SFixed] =
    new FixedEncoder[Long @@ SFixed] {
      def isDefault(a: Long @@ SFixed): Boolean                       = a == 0L
      def writeTo(a: Long @@ SFixed, output: CodedOutputStream): Unit = output.writeSFixed64NoTag(a)
      def writeToTag(fieldNumber: Int, a: Long @@ SFixed, output: CodedOutputStream): Unit =
        output.writeSFixed64(fieldNumber, a)
      def sizeTag(fieldNumber: Int, a: Long @@ SFixed): Int =
        CodedOutputStream.computeSFixed64Size(fieldNumber, a)
      def size(a: Long @@ SFixed): Int   = CodedOutputStream.computeSFixed64SizeNoTag(a)
      final protected val fixedSize: Int = 8
    }

  implicit final val encodeBoolean: Encoder[Boolean] = new FixedEncoder[Boolean] {
    def isDefault(a: Boolean): Boolean                       = !a
    def writeTo(a: Boolean, output: CodedOutputStream): Unit = output.writeBoolNoTag(a)
    def writeToTag(fieldNumber: Int, a: Boolean, output: CodedOutputStream): Unit =
      output.writeBool(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Boolean): Int =
      CodedOutputStream.computeBoolSize(fieldNumber, a)
    def size(a: Boolean): Int          = CodedOutputStream.computeBoolSizeNoTag(a)
    final protected val fixedSize: Int = 1
  }

  implicit final val encodeDouble: Encoder[Double] = new FixedEncoder[Double] {
    def isDefault(a: Double): Boolean                       = a == 0d
    def writeTo(a: Double, output: CodedOutputStream): Unit = output.writeDoubleNoTag(a)
    def writeToTag(fieldNumber: Int, a: Double, output: CodedOutputStream): Unit =
      output.writeDouble(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Double): Int =
      CodedOutputStream.computeDoubleSize(fieldNumber, a)
    def size(a: Double): Int           = CodedOutputStream.computeDoubleSizeNoTag(a)
    final protected val fixedSize: Int = 8
  }

  implicit final val encodeFloat: Encoder[Float] = new FixedEncoder[Float] {
    def isDefault(a: Float): Boolean                       = a == 0f
    def writeTo(a: Float, output: CodedOutputStream): Unit = output.writeFloatNoTag(a)
    def writeToTag(fieldNumber: Int, a: Float, output: CodedOutputStream): Unit =
      output.writeFloat(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Float): Int =
      CodedOutputStream.computeFloatSize(fieldNumber, a)
    def size(a: Float): Int            = CodedOutputStream.computeFloatSizeNoTag(a)
    final protected val fixedSize: Int = 4
  }

  implicit final val encodeString: Encoder[String] = new Encoder[String] {
    def isDefault(a: String): Boolean                       = a.isEmpty
    def writeTo(a: String, output: CodedOutputStream): Unit = output.writeStringNoTag(a)
    def writeToTag(fieldNumber: Int, a: String, output: CodedOutputStream): Unit =
      output.writeString(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: String): Int =
      CodedOutputStream.computeStringSize(fieldNumber, a)
    def size(a: String): Int = CodedOutputStream.computeStringSizeNoTag(a)
  }

  implicit final val encodeByteString: Encoder[ByteString] = new Encoder[ByteString] {
    def isDefault(a: ByteString): Boolean                       = a.isEmpty
    def writeTo(a: ByteString, output: CodedOutputStream): Unit = output.writeBytesNoTag(a)
    def writeToTag(fieldNumber: Int, a: ByteString, output: CodedOutputStream): Unit =
      output.writeBytes(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: ByteString): Int =
      CodedOutputStream.computeBytesSize(fieldNumber, a)
    def size(a: ByteString): Int = CodedOutputStream.computeBytesSizeNoTag(a)
  }

  implicit final val encodeByteArray: Encoder[Array[Byte]] = new Encoder[Array[Byte]] {
    def isDefault(a: Array[Byte]): Boolean                       = a.isEmpty
    def writeTo(a: Array[Byte], output: CodedOutputStream): Unit = output.writeByteArrayNoTag(a)
    def writeToTag(fieldNumber: Int, a: Array[Byte], output: CodedOutputStream): Unit =
      output.writeByteArray(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: Array[Byte]): Int =
      CodedOutputStream.computeByteArraySize(fieldNumber, a)
    def size(a: Array[Byte]): Int = CodedOutputStream.computeByteArraySizeNoTag(a)
  }

  implicit final val encodeByteBuffer: Encoder[ByteBuffer] = new Encoder[ByteBuffer] {
    def isDefault(a: ByteBuffer): Boolean                       = a.capacity() == 0
    def writeTo(a: ByteBuffer, output: CodedOutputStream): Unit = output.writeRawBytes(a)
    def writeToTag(fieldNumber: Int, a: ByteBuffer, output: CodedOutputStream): Unit =
      output.writeByteBuffer(fieldNumber, a)
    def sizeTag(fieldNumber: Int, a: ByteBuffer): Int =
      CodedOutputStream.computeByteBufferSize(fieldNumber, a)
    def size(a: ByteBuffer): Int = CodedOutputStream.computeByteBufferSizeNoTag(a)
  }

  final def encodeEnumeration[E <: Enumeration]: Encoder[E#Value] =
    new NumericEncoder[E#Value] {
      def isDefault(a: E#Value): Boolean                       = a.id == 0
      def writeTo(a: E#Value, output: CodedOutputStream): Unit = output.writeEnumNoTag(a.id)
      def writeToTag(fieldNumber: Int, a: E#Value, output: CodedOutputStream): Unit =
        output.writeEnum(fieldNumber, a.id)
      def sizeTag(fieldNumber: Int, a: E#Value): Int =
        CodedOutputStream.computeEnumSize(fieldNumber, a.id)
      def size(a: E#Value): Int = CodedOutputStream.computeEnumSizeNoTag(a.id)
    }

  implicit final def encodeOption[A](implicit e: FieldEncoder[A]): Encoder[Option[A]] =
    new MessageEncoder[Option[A]] {
      def writeTo(oa: Option[A], output: CodedOutputStream): Unit = oa match {
        case Some(a) => e.writeToTag(1, a, output)
        case None    =>
      }
      def size(oa: Option[A]): Int = oa match {
        case Some(a) => e.sizeTag(1, a)
        case None    => 0
      }
      def isDefault(oa: Option[A]): Boolean = oa.isEmpty
    }

  implicit final def encodeSome[A](implicit e: FieldEncoder[A]): Encoder[Some[A]] =
    new MessageEncoder[Some[A]] {
      def writeTo(a: Some[A], output: CodedOutputStream): Unit = e.writeToTag(1, a.get, output)
      def size(a: Some[A]): Int                                = e.sizeTag(1, a.get)
      def isDefault(oa: Some[A]): Boolean                      = false
    }

  implicit final val encodeNone: Encoder[None.type] =
    new MessageEncoder[None.type] {
      def writeTo(oa: None.type, output: CodedOutputStream): Unit = ()
      def size(oa: None.type): Int                                = 0
      def isDefault(oa: None.type): Boolean                       = true
    }

  implicit final def encodeEither[L, R](implicit
    l: FieldEncoder[L],
    r: FieldEncoder[R]
  ): Encoder[Either[L, R]] =
    new MessageEncoder[Either[L, R]] {
      def writeTo(e: Either[L, R], output: CodedOutputStream): Unit = e match {
        case Left(lv)  => l.writeToTag(1, lv, output)
        case Right(rv) => r.writeToTag(2, rv, output)
      }
      def size(e: Either[L, R]): Int = e match {
        case Left(lv)  => l.sizeTag(1, lv)
        case Right(rv) => r.sizeTag(2, rv)
      }
      def isDefault(e: Either[L, R]): Boolean = false
    }

  // encoding
  implicit final val encodeBigDecimal: Encoder[BigDecimal] = encodeString.contramap(_.toString())
  implicit final val encodeBigInt: Encoder[BigInt]         = encodeString.contramap(_.toString())
  implicit final val encodeShort: Encoder[Short]           = encodeInt.contramap(_.toInt)
  implicit final val encodeChar: Encoder[Char]             = encodeInt.contramap(_.toInt)
}
