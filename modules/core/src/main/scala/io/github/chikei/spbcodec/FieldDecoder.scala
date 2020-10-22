package io.github.chikei.spbcodec

import java.util.UUID

import com.google.protobuf.ByteString
import io.github.chikei.spbcodec.Decoder.Result
import io.github.chikei.spbcodec.FieldDecoder.SimpleFieldDecoder
import io.github.chikei.spbcodec.WireDecoder.{
  MessageDecoder,
  OneofDecoder,
  RepeatedMessageDecoder
}
import io.github.chikei.spbcodec.error.{
  DecodingFailure,
  MissingField,
  MissingFieldDecoder,
  OneofNotSet,
  UnknownSubtypeIndex,
  WrongWireDecoderType
}
import io.github.chikei.spbcodec.tag.{ @@, Fixed, SFixed, Signed, Unsigned }

import scala.annotation.implicitNotFound
import scala.collection.Factory
import scala.util.{ Failure, Success, Try }

@implicitNotFound("No FieldDecoder found for type ${A}.")
trait FieldDecoder[A] { self =>
  def wireDecoder: WireDecoder
  def build(wireDecoder: WireDecoder): Result[A]
  def emap[B](f: A => Result[B]): FieldDecoder[B] =
    new FieldDecoder.MappedFieldDecoder[B, A](self, f)
  def emapTry[B](f: A => Try[B]): FieldDecoder[B] =
    new FieldDecoder.MappedFieldDecoder[B, A](
      self,
      (a: A) =>
        f(a) match {
          case Failure(exception) => Left(DecodingFailure.fromThrowable(exception, ""))
          case Success(value)     => Right(value)
        }
    )
}

object FieldDecoder extends MidPriorityFieldDecoder {

  def apply[A](implicit decoder: FieldDecoder[A]): FieldDecoder[A] = decoder

  private class MappedFieldDecoder[To, From](
    val base: FieldDecoder[From],
    val f: From => Result[To]
  ) extends FieldDecoder[To] {
    def wireDecoder: WireDecoder = base.wireDecoder

    def build(wireDecoder: WireDecoder): Result[To] = base.build(wireDecoder).flatMap(f)

    override def emap[C](g: To => Result[C]): FieldDecoder[C] =
      new MappedFieldDecoder[C, From](base, e => f(e).flatMap(g))
  }

  abstract class SimpleFieldDecoder[A] extends FieldDecoder[A] {
    final def build(wireDecoder: WireDecoder): Result[A] =
      buildPf.applyOrElse(
        wireDecoder,
        (_: WireDecoder) => Left(WrongWireDecoderType(typename, wireDecoder))
      )
    protected def buildPf: PartialFunction[WireDecoder, Result[A]]
    protected def typename: String
  }

  // decoders for protobuf primitive types
  implicit final val decodeInt: FieldDecoder[Int] = new SimpleFieldDecoder[Int] {
    def wireDecoder: WireDecoder = new WireDecoder.IntDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[Int]] = {
      case d: WireDecoder.IntDecoder => Right(d.value)
    }
    protected val typename: String = "IntDecoder"
  }

  implicit final val decodeUnsignedInt: FieldDecoder[Int @@ Unsigned] =
    new SimpleFieldDecoder[Int @@ Unsigned] {
      def wireDecoder: WireDecoder = new WireDecoder.UnsignedIntDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[Int @@ Unsigned]] = {
        case d: WireDecoder.UnsignedIntDecoder => Right(d.value)
      }
      protected val typename: String = "UnsignedIntDecoder"
    }

  implicit final val decodeSignedInt: FieldDecoder[Int @@ Signed] =
    new SimpleFieldDecoder[Int @@ Signed] {
      def wireDecoder: WireDecoder = new WireDecoder.SignedIntDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[Int @@ Signed]] = {
        case d: WireDecoder.SignedIntDecoder => Right(d.value)
      }
      protected val typename: String = "SignedIntDecoder"
    }

  implicit final val decodeFixedInt: FieldDecoder[Int @@ Fixed] =
    new SimpleFieldDecoder[Int @@ Fixed] {
      def wireDecoder: WireDecoder = new WireDecoder.FixedIntDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[Int @@ Fixed]] = {
        case d: WireDecoder.FixedIntDecoder => Right(d.value)
      }
      protected val typename: String = "FixedIntDecoder"
    }

  implicit final val decodeFixedSignedInt: FieldDecoder[Int @@ SFixed] =
    new SimpleFieldDecoder[Int @@ SFixed] {
      def wireDecoder: WireDecoder = new WireDecoder.SignedFixedIntDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[Int @@ SFixed]] = {
        case d: WireDecoder.SignedFixedIntDecoder => Right(d.value)
      }
      protected val typename: String = "FixedSignedIntDecoder"
    }

  implicit final val decodeLong: FieldDecoder[Long] = new SimpleFieldDecoder[Long] {
    def wireDecoder: WireDecoder = new WireDecoder.LongDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[Long]] = {
      case d: WireDecoder.LongDecoder => Right(d.value)
    }
    protected val typename: String = "LongDecoder"
  }

  implicit final val decodeUnsignedLong: FieldDecoder[Long @@ Unsigned] =
    new SimpleFieldDecoder[Long @@ Unsigned] {
      def wireDecoder: WireDecoder = new WireDecoder.UnsignedLongDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[Long @@ Unsigned]] = {
        case d: WireDecoder.UnsignedLongDecoder => Right(d.value)
      }
      protected val typename: String = "UnsignedLongDecoder"
    }

  implicit final val decodeSignedLong: FieldDecoder[Long @@ Signed] =
    new SimpleFieldDecoder[Long @@ Signed] {
      def wireDecoder: WireDecoder = new WireDecoder.SignedLongDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[Long @@ Signed]] = {
        case d: WireDecoder.SignedLongDecoder => Right(d.value)
      }
      protected val typename: String = "SignedLongDecoder"
    }

  implicit final val decodeFixedLong: FieldDecoder[Long @@ Fixed] =
    new SimpleFieldDecoder[Long @@ Fixed] {
      def wireDecoder: WireDecoder = new WireDecoder.FixedLongDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[Long @@ Fixed]] = {
        case d: WireDecoder.FixedLongDecoder => Right(d.value)
      }
      protected val typename: String = "FixedLongDecoder"
    }

  implicit final val decodeFixedSignedLong: FieldDecoder[Long @@ SFixed] =
    new SimpleFieldDecoder[Long @@ SFixed] {
      def wireDecoder: WireDecoder = new WireDecoder.SignedFixedLongDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[Long @@ SFixed]] = {
        case d: WireDecoder.SignedFixedLongDecoder => Right(d.value)
      }
      protected val typename: String = "FixedSignedLongDecoder"
    }

  implicit final val decodeBoolean: FieldDecoder[Boolean] = new SimpleFieldDecoder[Boolean] {
    def wireDecoder: WireDecoder = new WireDecoder.BooleanDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[Boolean]] = {
      case d: WireDecoder.BooleanDecoder => Right(d.value)
    }
    protected val typename: String = "BooleanDecoder"
  }

  implicit final val decodeFloat: FieldDecoder[Float] = new SimpleFieldDecoder[Float] {
    def wireDecoder: WireDecoder = new WireDecoder.FloatDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[Float]] = {
      case d: WireDecoder.FloatDecoder => Right(d.value)
    }
    protected val typename: String = "FloatDecoder"
  }

  implicit final val decodeDouble: FieldDecoder[Double] = new SimpleFieldDecoder[Double] {
    def wireDecoder: WireDecoder = new WireDecoder.DoubleDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[Double]] = {
      case d: WireDecoder.DoubleDecoder => Right(d.value)
    }
    protected val typename: String = "DoubleDecoder"
  }

  implicit final val decodeString: FieldDecoder[String] = new SimpleFieldDecoder[String] {
    def wireDecoder: WireDecoder = new WireDecoder.StringDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[String]] = {
      case d: WireDecoder.StringDecoder => Right(d.value)
    }
    protected val typename: String = "StringDecoder"
  }

  implicit final val decodeByteString: FieldDecoder[ByteString] =
    new SimpleFieldDecoder[ByteString] {
      def wireDecoder: WireDecoder = new WireDecoder.ByteStringDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[ByteString]] = {
        case d: WireDecoder.ByteStringDecoder => Right(d.value)
      }
      protected val typename: String = "ByteStringDecoder"
    }

  final def decodeEnumeration[E <: Enumeration](enum: E): FieldDecoder[E#Value] =
    new SimpleFieldDecoder[E#Value] {
      def wireDecoder: WireDecoder = new WireDecoder.EnumDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[E#Value]] = {
        case d: WireDecoder.EnumDecoder =>
          val v = d.value
          Try(enum(v)).toEither.left.map(_ =>
            new DecodingFailure(s"Unknown value $v for enum $enum")
          )
      }
      protected val typename: String = "EnumDecoder"
    }

  implicit final def decodeIntCollection[C[_]](implicit
    factory: Factory[Int, C[Int]]
  ): FieldDecoder[C[Int]] = new SimpleFieldDecoder[C[Int]] {
    def wireDecoder: WireDecoder = new WireDecoder.RepeatedIntDecoder

    protected val buildPf: PartialFunction[WireDecoder, Result[C[Int]]] = {
      case d: WireDecoder.RepeatedIntDecoder => Right(d.value.to(factory))
    }
    protected val typename: String = "RepeatedIntDecoder"
  }

  implicit final def decodeUnsignedIntCollection[C[_]](implicit
    factory: Factory[Int @@ Unsigned, C[Int @@ Unsigned]]
  ): FieldDecoder[C[Int @@ Unsigned]] =
    new SimpleFieldDecoder[C[Int @@ Unsigned]] {
      def wireDecoder: WireDecoder = new WireDecoder.RepeatedUnsignedIntDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[C[Int @@ Unsigned]]] = {
        case d: WireDecoder.RepeatedUnsignedIntDecoder => Right(d.value.to(factory))
      }
      protected val typename: String = "RepeatedUnsignedIntDecoder"
    }

  implicit final def decodeSignedIntCollection[C[_]](implicit
    factory: Factory[Int @@ Signed, C[Int @@ Signed]]
  ): FieldDecoder[C[Int @@ Signed]] =
    new SimpleFieldDecoder[C[Int @@ Signed]] {
      def wireDecoder: WireDecoder = new WireDecoder.RepeatedSignedIntDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[C[Int @@ Signed]]] = {
        case d: WireDecoder.RepeatedSignedIntDecoder => Right(d.value.to(factory))
      }
      protected val typename: String = "RepeatedSignedIntDecoder"
    }

  implicit final def decodeFixedIntCollection[C[_]](implicit
    factory: Factory[Int @@ Fixed, C[Int @@ Fixed]]
  ): FieldDecoder[C[Int @@ Fixed]] =
    new SimpleFieldDecoder[C[Int @@ Fixed]] {
      def wireDecoder: WireDecoder = new WireDecoder.RepeatedFixedIntDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[C[Int @@ Fixed]]] = {
        case d: WireDecoder.RepeatedFixedIntDecoder => Right(d.value.to(factory))
      }
      protected val typename: String = "RepeatedFixedIntDecoder"
    }

  implicit final def decodeFixedSignedIntCollection[C[_]](implicit
    factory: Factory[Int @@ SFixed, C[Int @@ SFixed]]
  ): FieldDecoder[C[Int @@ SFixed]] =
    new SimpleFieldDecoder[C[Int @@ SFixed]] {
      def wireDecoder: WireDecoder = new WireDecoder.RepeatedSignedFixedIntDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[C[Int @@ SFixed]]] = {
        case d: WireDecoder.RepeatedSignedFixedIntDecoder => Right(d.value.to(factory))
      }
      protected val typename: String = "RepeatedFixedSignedIntDecoder"
    }

  implicit final def decodeLongCollection[C[_]](implicit
    factory: Factory[Long, C[Long]]
  ): FieldDecoder[C[Long]] = new SimpleFieldDecoder[C[Long]] {
    def wireDecoder: WireDecoder = new WireDecoder.RepeatedLongDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[C[Long]]] = {
      case d: WireDecoder.RepeatedLongDecoder => Right(d.value.to(factory))
    }
    protected val typename: String = "RepeatedLongDecoder"
  }

  implicit final def decodeUnsignedLongCollection[C[_]](implicit
    factory: Factory[Long @@ Unsigned, C[Long @@ Unsigned]]
  ): FieldDecoder[C[Long @@ Unsigned]] =
    new SimpleFieldDecoder[C[Long @@ Unsigned]] {
      def wireDecoder: WireDecoder = new WireDecoder.RepeatedUnsignedLongDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[C[Long @@ Unsigned]]] = {
        case d: WireDecoder.RepeatedUnsignedLongDecoder => Right(d.value.to(factory))
      }
      protected val typename: String = "RepeatedUnsignedLongDecoder"
    }

  implicit final def decodeSignedLongCollection[C[_]](implicit
    factory: Factory[Long @@ Signed, C[Long @@ Signed]]
  ): FieldDecoder[C[Long @@ Signed]] =
    new SimpleFieldDecoder[C[Long @@ Signed]] {
      def wireDecoder: WireDecoder = new WireDecoder.RepeatedSignedLongDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[C[Long @@ Signed]]] = {
        case d: WireDecoder.RepeatedSignedLongDecoder => Right(d.value.to(factory))
      }
      protected val typename: String = "RepeatedSignedLongDecoder"
    }

  implicit final def decodeFixedLongCollection[C[_]](implicit
    factory: Factory[Long @@ Fixed, C[Long @@ Fixed]]
  ): FieldDecoder[C[Long @@ Fixed]] =
    new SimpleFieldDecoder[C[Long @@ Fixed]] {
      def wireDecoder: WireDecoder = new WireDecoder.RepeatedFixedLongDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[C[Long @@ Fixed]]] = {
        case d: WireDecoder.RepeatedFixedLongDecoder => Right(d.value.to(factory))
      }
      protected val typename: String = "RepeatedFixedLongDecoder"
    }

  implicit final def decodeFixedSignedLongCollection[C[_]](implicit
    factory: Factory[Long @@ SFixed, C[Long @@ SFixed]]
  ): FieldDecoder[C[Long @@ SFixed]] =
    new SimpleFieldDecoder[C[Long @@ SFixed]] {
      def wireDecoder: WireDecoder = new WireDecoder.RepeatedSignedFixedLongDecoder
      protected val buildPf: PartialFunction[WireDecoder, Result[C[Long @@ SFixed]]] = {
        case d: WireDecoder.RepeatedSignedFixedLongDecoder => Right(d.value.to(factory))
      }
      protected val typename: String = "RepeatedFixedSignedLongDecoder"
    }

  implicit final def decodeBooleanCollection[C[_]](implicit
    factory: Factory[Boolean, C[Boolean]]
  ): FieldDecoder[C[Boolean]] = new SimpleFieldDecoder[C[Boolean]] {
    def wireDecoder: WireDecoder = new WireDecoder.RepeatedBooleanDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[C[Boolean]]] = {
      case d: WireDecoder.RepeatedBooleanDecoder => Right(d.value.to(factory))
    }
    protected val typename: String = "RepeatedBooleanDecoder"
  }

  implicit final def decodeFloatCollection[C[_]](implicit
    factory: Factory[Float, C[Float]]
  ): FieldDecoder[C[Float]] = new SimpleFieldDecoder[C[Float]] {
    def wireDecoder: WireDecoder = new WireDecoder.RepeatedFloatDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[C[Float]]] = {
      case d: WireDecoder.RepeatedFloatDecoder => Right(d.value.to(factory))
    }
    protected val typename: String = "RepeatedFloatDecoder"
  }

  implicit final def decodeDoubleCollection[C[_]](implicit
    factory: Factory[Double, C[Double]]
  ): FieldDecoder[C[Double]] = new SimpleFieldDecoder[C[Double]] {
    def wireDecoder: WireDecoder = new WireDecoder.RepeatedDoubleDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[C[Double]]] = {
      case d: WireDecoder.RepeatedDoubleDecoder => Right(d.value.to(factory))
    }
    protected val typename: String = "RepeatedDoubleDecoder"
  }

  implicit final def decodeStringCollection[C[_]](implicit
    factory: Factory[String, C[String]]
  ): FieldDecoder[C[String]] = new SimpleFieldDecoder[C[String]] {
    def wireDecoder: WireDecoder = new WireDecoder.RepeatedStringDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[C[String]]] = {
      case d: WireDecoder.RepeatedStringDecoder => Right(d.value.to(factory))
    }
    protected val typename: String = "RepeatedStringDecoder"
  }

  implicit final def decodeByteStringCollection[C[_]](implicit
    factory: Factory[ByteString, C[ByteString]]
  ): FieldDecoder[C[ByteString]] = new SimpleFieldDecoder[C[ByteString]] {
    def wireDecoder: WireDecoder = new WireDecoder.RepeatedByteStringDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[C[ByteString]]] = {
      case d: WireDecoder.RepeatedByteStringDecoder => Right(d.value.to(factory))
    }
    protected val typename: String = "RepeatedByteStringDecoder"
  }

  final def decodeEnumerationCollection[E <: Enumeration, C[_]](enum: E)(implicit
    factory: Factory[E#Value, C[E#Value]]
  ): FieldDecoder[C[E#Value]] = new SimpleFieldDecoder[C[E#Value]] {
    def wireDecoder: WireDecoder = new WireDecoder.RepeatedEnumDecoder
    protected val buildPf: PartialFunction[WireDecoder, Result[C[E#Value]]] = {
      case d: WireDecoder.RepeatedEnumDecoder =>
        val vs = d.value
        var v  = 0
        var i  = 0
        Try {
          val builder = factory.newBuilder
          val it      = vs.iterator
          while (it.hasNext) {
            v = it.next()
            builder.addOne(enum(v))
            i += 1
          }
          builder.result()
        }.toEither.left.map(_ =>
          new DecodingFailure(s"Unknown value $v for enum $enum in ${i}th repeated message.")
        )
    }
    protected val typename: String = "RepeatedEnumDecoder"
  }

  final def decodeCollection[From, To, C[From] <: IterableOnce[From]](
    f: From => Result[To]
  )(implicit factory: Factory[To, C[To]], decoder: FieldDecoder[C[From]]): FieldDecoder[C[To]] =
    decoder.emap { col =>
      var last: Result[To] = null
      val it               = col.iterator
      val builder          = factory.newBuilder
      while (it.hasNext && (last == null || last.isRight)) {
        last = f(it.next())
        last.foreach(t => builder.addOne(t))
      }
      last.map(_ => builder.result())
    }

  implicit final def decodeOption[A](implicit d: FieldDecoder[A]): FieldDecoder[Option[A]] =
    new MessageFieldDecoder[Option[A]] {
      def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder =
        new MessageDecoder(Map(1 -> d.wireDecoder))

      def build(wireDecoder: WireDecoder): Result[Option[A]] =
        wireDecoder match {
          case msg: MessageDecoder =>
            msg.value.get(1) match {
              case Some(decoder) =>
                if (!msg.seen(1)) Right(None)
                else Right(d.build(decoder).toOption)
              case None => Left(MissingFieldDecoder(1, ""))
            }
          case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
        }
    }

  implicit final def decodeSome[A](implicit d: FieldDecoder[A]): FieldDecoder[Some[A]] =
    new MessageFieldDecoder[Some[A]] {
      def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder =
        new MessageDecoder(Map(1 -> d.wireDecoder))

      def build(wireDecoder: WireDecoder): Result[Some[A]] =
        wireDecoder match {
          case msg: MessageDecoder =>
            msg.value.get(1) match {
              case Some(decoder) =>
                if (!msg.seen(1)) Left(MissingField(1, "Option"))
                else d.build(decoder).map(v => Some(v))
              case None => Left(MissingFieldDecoder(1, ""))
            }
          case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
        }
    }

  implicit final val decodeNone: FieldDecoder[None.type] = new MessageFieldDecoder[None.type] {
    def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder = new MessageDecoder(Map.empty)

    def build(wireDecoder: WireDecoder): Result[None.type] =
      wireDecoder match {
        case msg: MessageDecoder =>
          if (msg.seen.isEmpty) Right(None)
          else Left(DecodingFailure("None"))
        case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
      }
  }

  implicit final def decodeEither[L, R](implicit
    l: FieldDecoder[L],
    r: FieldDecoder[R]
  ): FieldDecoder[Either[L, R]] = new MessageFieldDecoder[Either[L, R]] {
    def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder =
      new OneofDecoder(Map(1 -> (() => l.wireDecoder), 2 -> (() => r.wireDecoder)))

    def build(wireDecoder: WireDecoder): Result[Either[L, R]] = wireDecoder match {
      case oneof: OneofDecoder =>
        (oneof.lastDecoder, oneof.lastOneof) match {
          case (_, 0)                 => Left(OneofNotSet("Either"))
          case (Some(wireDecoder), 1) => l.build(wireDecoder).map(l => Left(l))
          case (Some(wireDecoder), 2) => r.build(wireDecoder).map(r => Right(r))
          case _                      => Left(UnknownSubtypeIndex(oneof.lastOneof, "Either"))
        }
      case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
    }
  }

  // decoding
  implicit final val decodeBigDecimal: FieldDecoder[BigDecimal] =
    decodeString.emapTry(v => Try(BigDecimal(v)))
  implicit final val decodeBigInt: FieldDecoder[BigInt] = decodeString.emapTry(v => Try(BigInt(v)))
  implicit final val decodeShort: FieldDecoder[Short]   = decodeInt.emap(i => Right(i.toShort))
  implicit final val decodeChar: FieldDecoder[Char]     = decodeInt.emap(i => Right(i.toChar))

  implicit final val encodeUUID: FieldDecoder[UUID] = FieldDecoder[List[Long @@ Signed]].emap {
    case m :: l :: Nil => Right(new UUID(m, l))
    case other         => Left(DecodingFailure(s"$other must have size 2 to create an UUID from long's"))
  }
}

trait MidPriorityFieldDecoder {
  implicit final def decodeMessageCollection[A, C[_]](implicit
    fieldDecoder: MessageFieldDecoder[A],
    factory: Factory[A, C[A]]
  ): FieldDecoder[C[A]] = new SimpleFieldDecoder[C[A]] {
    def wireDecoder: WireDecoder = new RepeatedMessageDecoder[A](fieldDecoder)
    protected val buildPf: PartialFunction[WireDecoder, Result[C[A]]] = {
      case d: WireDecoder.RepeatedMessageDecoder[A @unchecked] if d.fieldDecoder == fieldDecoder =>
        Right(d.value.to(factory))
    }
    protected val typename: String = "RepeatedMessageDecoder"
  }
}
