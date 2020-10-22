package io.github.chikei.spbcodec

import java.util.UUID

import com.google.protobuf.ByteString
import io.github.chikei.spbcodec.instances.ArbitraryInstances
import io.github.chikei.spbcodec.tag.{ @@, Fixed, Signed, Unsigned }
import io.github.chikei.spbcodec.tests.ProtolessSuite
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

case class Budget(budget: Long) extends AnyVal
case class Id(value: UUID)      extends AnyVal

class FieldEncodingCheck
    extends ProtolessSuite
    with ScalaCheckDrivenPropertyChecks
    with ArbitraryInstances {

  "Field encoding must pass property testing" - {

    "Int" in {
      check[Int]
    }

    "Long" in {
      check[Long]
    }

    "Float" in {
      check[Float]
    }

    "Double" in {
      check[Double]
    }

    "String" in {
      check[String]
    }

    "Boolean" in {
      check[Boolean]
    }

    "ByteString" in {
      check[ByteString]
    }

    "unsigned int32" in {
      check[Int @@ Unsigned]
    }

    "unsigned int64" in {
      check[Long @@ Unsigned]
    }

    "signed int32" in {
      check[Int @@ Signed]
    }

    "signed int64" in {
      check[Long @@ Signed]
    }

    "fixed int32" in {
      check[Int @@ Fixed]
    }

    "fixed int64" in {
      check[Long @@ Fixed]
    }

    "fixed signed int32" in {
      check[Int @@ Signed with Fixed]
    }

    "fixed signed int64" in {
      check[Long @@ Signed with Fixed]
    }

    "Short" in { check[Short] }
    "Char" in { check[Char] }
    "BigDecimal" in { check[BigDecimal] }
    "BigInt" in { check[BigInt] }
    "UUID" in { check[UUID] }

    "Option[Int]" in {
      check[Option[Int]]
    }

    "Option[String]" in {
      check[Option[String]]
    }

    "Either[Int, String]" in {
      check[Either[Int, String]]
    }
  }

  val indexGenerator: Gen[Int] = Gen.choose(1, 1000)

  private def check[T](implicit
    enc: FieldEncoder[T],
    dec: FieldDecoder[T],
    arbitrary: Arbitrary[T]
  ) =
    forAll(indexGenerator) { index =>
      whenever(index >= 1 && index <= 1000) {
        forAll((t: T) => encodeDecodeField(t, index))
      }
    }

  private def encodeDecodeField[T](
    t: T,
    index: Int
  )(implicit enc: FieldEncoder[T], dec: FieldDecoder[T]) = {
    val bytesEncoded    = enc.encodeAsBytes(index, t)
    val entityDecoded   = dec.decode(bytesEncoded, index).toOption.get
    val entityReEncoded = enc.encodeAsBytes(index, entityDecoded)

    entityReEncoded.toList.must(===(bytesEncoded.toList))
  }
}
