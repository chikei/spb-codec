package io.github.chikei.spbcodec.magnolia

import io.github.chikei.spbcodec.{ Decoder, Encoder, EncoderDecoderAssertions }
import io.github.chikei.spbcodec.tests.ProtolessSuite
import io.github.chikei.spbcodec.tests.instances.EqualityInstances
import io.github.chikei.spbcodec.tests.samples._

class SemiAutoEncoderDecoderSuite
    extends ProtolessSuite
    with EqualityInstances
    with EncoderDecoderAssertions {

  import encoding.semiauto._
  import decoding.semiauto._

  implicit val decoderTestCaseAllFields: Decoder[TestCaseAllFields] =
    deriveDecoder[TestCaseAllFields]
  implicit val decoderTestCaseOptionalFieldsOptionalCaseObject
    : Decoder[TestCaseOptionalFields.TestOptionalCaseObject.type] =
    deriveDecoder[TestCaseOptionalFields.TestOptionalCaseObject.type]
  implicit val decoderTestCaseOptionalFields: Decoder[TestCaseOptionalFields] =
    deriveDecoder[TestCaseOptionalFields]
  implicit val decoderTestCaseRepeatedFields: Decoder[TestCaseRepeatedFields] =
    deriveDecoder[TestCaseRepeatedFields]
  implicit val decoderTestCaseEmptyRepeated: Decoder[TestCaseEmptyRepeated] =
    deriveDecoder[TestCaseEmptyRepeated]
  implicit val decoderTestCaseCollections: Decoder[TestCaseCollections] =
    deriveDecoder[TestCaseCollections]
  implicit val decoderTestCaseCustomType: Decoder[TestCaseCustomType] =
    deriveDecoder[TestCaseCustomType]
  implicit val decoderTestCaseNestedNestedInner: Decoder[TestCaseNested.InnerNested] =
    deriveDecoder[TestCaseNested.InnerNested]
  implicit val decoderTestCaseNested: Decoder[TestCaseNested] = deriveDecoder[TestCaseNested]
  implicit val decoderTestCaseOneofIntValue: Decoder[TestCaseOneof.IntValue] =
    deriveDecoder[TestCaseOneof.IntValue]
  implicit val decoderTestCaseOneofCustomType: Decoder[TestCaseOneof.CustomType] =
    deriveDecoder[TestCaseOneof.CustomType]
  implicit val decoderTestCaseOneof: Decoder[TestCaseOneof]        = deriveDecoder[TestCaseOneof]
  implicit val decoderTestCaseCaseObject: Decoder[CaseObject.type] = deriveDecoder[CaseObject.type]

  implicit val encoderTestCaseAllFields: Encoder[TestCaseAllFields] =
    deriveEncoder[TestCaseAllFields]
  implicit val encoderTestCaseOptionalFieldsOptionalCaseObject
    : Encoder[TestCaseOptionalFields.TestOptionalCaseObject.type] =
    deriveEncoder[TestCaseOptionalFields.TestOptionalCaseObject.type]
  implicit val encoderTestCaseOptionalFields: Encoder[TestCaseOptionalFields] =
    deriveEncoder[TestCaseOptionalFields]
  implicit val encoderTestCaseRepeatedFields: Encoder[TestCaseRepeatedFields] =
    deriveEncoder[TestCaseRepeatedFields]
  implicit val encoderTestCaseEmptyRepeated: Encoder[TestCaseEmptyRepeated] =
    deriveEncoder[TestCaseEmptyRepeated]
  implicit val encoderTestCaseCollections: Encoder[TestCaseCollections] =
    deriveEncoder[TestCaseCollections]
  implicit val encoderTestCaseCustomType: Encoder[TestCaseCustomType] =
    deriveEncoder[TestCaseCustomType]
  implicit val encoderTestCaseNestedNestedInner: Encoder[TestCaseNested.InnerNested] =
    deriveEncoder[TestCaseNested.InnerNested]
  implicit val encoderTestCaseNested: Encoder[TestCaseNested] = deriveEncoder[TestCaseNested]
  implicit val encoderTestCaseOneofIntValue: Encoder[TestCaseOneof.IntValue] =
    deriveEncoder[TestCaseOneof.IntValue]
  implicit val encoderTestCaseOneofCustomType: Encoder[TestCaseOneof.CustomType] =
    deriveEncoder[TestCaseOneof.CustomType]
  implicit val encoderTestCaseOneof: Encoder[TestCaseOneof]        = deriveEncoder[TestCaseOneof]
  implicit val encoderTestCaseCaseObject: Encoder[CaseObject.type] = deriveEncoder[CaseObject.type]

  "SemiAuto Encoders must convert case class to protobuf format for" - {
    "protobuf native fields type" in {
      testEncoding(TestCaseAllFields)
    }
    "optional fields" in {
      testEncoding(TestCaseOptionalFields)
    }

    "repeated fields" in {
      testEncoding(TestCaseRepeatedFields)
    }

    "repeated fields with gap" in {
      testEncoding(TestCaseEmptyRepeated)
    }

    "repeated fields with 0 or 1 value" in {
      testEncoding(TestCaseRepeatedFieldsOneValue)
    }

    "every scala collections" in {
      testEncoding(TestCaseCollections)
    }

    "custom types (uuid, bigdecimal, char)" in {
      testEncoding(TestCaseCustomType)
    }

    "nested fields" in {
      testEncoding(TestCaseNested)
    }

    "sealed trait" in {
      testEncoding(TestCaseOneof)
    }

    "case object" in {
      testEncoding(TestCaseCaseObject)
    }
  }

  "SemiAuto Decoders must convert protobuf format to case class for" - {
    "protobuf native fields type" in {
      testDecoding(TestCaseAllFields)
    }
    "optional fields" in {
      testDecoding(TestCaseOptionalFields)
    }

    "repeated fields" in {
      testDecoding(TestCaseRepeatedFields)
    }

    "repeated fields with gap" in {
      testDecoding(TestCaseEmptyRepeated)
    }

    "repeated fields with 0 or 1 value" in {
      testDecoding(TestCaseRepeatedFieldsOneValue)
    }

    "every scala collections" in {
      testDecoding(TestCaseCollections)
    }

    "custom types (uuid, bigdecimal, char)" in {
      testDecoding(TestCaseCustomType)
    }

    "nested fields" in {
      testDecoding(TestCaseNested)
    }

    "sealed trait" in {
      testDecoding(TestCaseOneof)
    }

    "case object" in {
      testDecoding(TestCaseCaseObject)
    }
  }

  "SemiAuto Encoders/Decoders must respect law: encode(i) === encode(decode(encode(i))" - {
    "protobuf native fields type" in {
      testFullCycle(TestCaseAllFields)
    }
    "optional fields" in {
      testFullCycle(TestCaseOptionalFields)
    }

    "repeated fields" in {
      testFullCycle(TestCaseRepeatedFields)
    }

    "repeated fields with gap" in {
      testFullCycle(TestCaseEmptyRepeated)
    }

    "repeated fields with 0 or 1 value" in {
      testFullCycle(TestCaseRepeatedFieldsOneValue)
    }

    "every scala collections" in {
      testFullCycle(TestCaseCollections)
    }

    "custom types (uuid, bigdecimal, char)" in {
      testFullCycle(TestCaseCustomType)
    }

    "nested fields" in {
      testFullCycle(TestCaseNested)
    }

    "sealed trait" in {
      testFullCycle(TestCaseOneof)
    }

    "case object" in {
      testFullCycle(TestCaseCaseObject)
    }
  }

}
