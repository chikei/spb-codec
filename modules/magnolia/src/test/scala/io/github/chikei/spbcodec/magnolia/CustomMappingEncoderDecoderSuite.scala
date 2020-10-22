package io.github.chikei.spbcodec.magnolia

import io.github.chikei.spbcodec.{ Decoder, Encoder, EncoderDecoderAssertions }
import io.github.chikei.spbcodec.tests.ProtolessSuite
import io.github.chikei.spbcodec.tests.instances.EqualityInstances
import io.github.chikei.spbcodec.tests.samples.TestCaseNestedCustomMapping.InnerNestedCustomMapping
import io.github.chikei.spbcodec.tests.samples._

class CustomMappingEncoderDecoderSuite
    extends ProtolessSuite
    with EqualityInstances
    with EncoderDecoderAssertions {

  import io.github.chikei.spbcodec.magnolia.decoding.semiauto._
  import io.github.chikei.spbcodec.magnolia.encoding.semiauto._

  implicit val indexSimple = {
    implicit val c = CustomIndex.fromMap[TestCaseCustomMappingSimple](Map(0 -> 2, 1 -> 5, 2 -> 16))
    Index.deriveIndex[TestCaseCustomMappingSimple]
  }

  implicit val decoderTestCaseCustomMappingSimple: Decoder[TestCaseCustomMappingSimple] =
    deriveDecoder[TestCaseCustomMappingSimple]

  implicit val encoderTestCaseCustomMappingSimple: Encoder[TestCaseCustomMappingSimple] =
    deriveEncoder[TestCaseCustomMappingSimple]

  implicit val indexRepeated = {
    implicit val c = CustomIndex.fromMap[TestCaseCustomMappingRepeated](
      Map(0 -> 3, 1 -> 8, 2 -> 13, 3 -> 14, 4 -> 16)
    )
    Index.deriveIndex[TestCaseCustomMappingRepeated]
  }

  implicit val decoderTestCaseCustomMappingRepeated: Decoder[TestCaseCustomMappingRepeated] =
    deriveDecoder[TestCaseCustomMappingRepeated]

  implicit val encoderTestCaseCustomMappingRepeated: Encoder[TestCaseCustomMappingRepeated] =
    deriveEncoder[TestCaseCustomMappingRepeated]

  implicit val indexInnerNestedCustomMapping = {
    implicit val c = CustomIndex.fromMap[InnerNestedCustomMapping](Map(0 -> 2, 1 -> 5))
    Index.deriveIndex[InnerNestedCustomMapping]
  }
  implicit val indexTestCaseNestedCustomMapping = {
    implicit val c = CustomIndex.fromMap[TestCaseNestedCustomMapping](Map(0 -> 3, 1 -> 4))
    Index.deriveIndex[TestCaseNestedCustomMapping]
  }
  implicit val decoderInnerNestedCustomMapping: Decoder[InnerNestedCustomMapping] =
    deriveDecoder[InnerNestedCustomMapping]

  implicit val decoderTestCaseNestedCustomMapping: Decoder[TestCaseNestedCustomMapping] =
    deriveDecoder[TestCaseNestedCustomMapping]

  implicit val encoderInnerNestedCustomMapping: Encoder[InnerNestedCustomMapping] =
    deriveEncoder[InnerNestedCustomMapping]

  implicit val encoderTestCaseNestedCustomMapping: Encoder[TestCaseNestedCustomMapping] =
    deriveEncoder[TestCaseNestedCustomMapping]

  implicit val indexTestCaseCustomMappingOneofOneOf = {
    implicit val c =
      CustomIndex.fromMap[TestCaseCustomMappingOneof.CustomType](Map(0 -> 2, 1 -> 5, 2 -> 16))
    Index.deriveIndex[TestCaseCustomMappingOneof.CustomType]
  }
  implicit val indexTestCaseCustomMappingOneof = {
    implicit val c = CustomIndex.fromMap[TestCaseCustomMappingOneof](Map(0 -> 3))
    Index.deriveIndex[TestCaseCustomMappingOneof]
  }
  implicit val decoderTestCaseCustomMappingOneofOneOf
    : Decoder[TestCaseCustomMappingOneof.CustomType] =
    deriveDecoder[TestCaseCustomMappingOneof.CustomType]

  implicit val decoderTestCaseCustomMappingOneof: Decoder[TestCaseCustomMappingOneof] =
    deriveDecoder[TestCaseCustomMappingOneof]

  implicit val encoderTestCaseCustomMappingOneofOneOf
    : Encoder[TestCaseCustomMappingOneof.CustomType] =
    deriveEncoder[TestCaseCustomMappingOneof.CustomType]

  implicit val encoderTestCaseCustomMappingOneof: Encoder[TestCaseCustomMappingOneof] =
    deriveEncoder[TestCaseCustomMappingOneof]

  "Encoder must convert case class to protobuf format for" - {
    "protobuf native fields type" in {
      testEncoding(TestCaseCustomMappingSimple)
    }

    "repeated fields" in {
      testEncoding(TestCaseCustomMappingRepeated)
    }

    "nested fields" in {
      testEncoding(TestCaseNestedCustomMapping)
    }

    "oneof fields" in {
      testEncoding(TestCaseCustomMappingOneof)
    }
  }

  "Decoder must convert protobuf format to case class for" - {
    "protobuf native fields type" in {
      testDecoding(TestCaseCustomMappingSimple)
    }

    "repeated fields" in {
      testDecoding(TestCaseCustomMappingRepeated)
    }

    "nested fields" in {
      testDecoding(TestCaseNestedCustomMapping)
    }

    "oneof fields" in {
      testDecoding(TestCaseCustomMappingOneof)
    }
  }

  "Encoders/Decoders must respect law: encode(i) === encode(decode(encode(i))" - {
    "protobuf native fields type" in {
      testFullCycle(TestCaseCustomMappingSimple)
    }

    "repeated fields" in {
      testFullCycle(TestCaseCustomMappingRepeated)
    }

    "nested fields" in {
      testFullCycle(TestCaseNestedCustomMapping)
    }

    "oneof fields" in {
      testFullCycle(TestCaseCustomMappingOneof)
    }
  }

}
