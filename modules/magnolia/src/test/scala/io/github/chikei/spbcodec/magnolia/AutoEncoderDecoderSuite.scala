package io.github.chikei.spbcodec.magnolia

import io.github.chikei.spbcodec.EncoderDecoderAssertions
import io.github.chikei.spbcodec.tests.ProtolessSuite
import io.github.chikei.spbcodec.tests.instances.EqualityInstances
import io.github.chikei.spbcodec.tests.samples._

class AutoEncoderDecoderSuite
    extends ProtolessSuite
    with EqualityInstances
    with EncoderDecoderAssertions {

  import decoding.auto._
  import encoding.auto._

  "Auto Encoder must convert case class to protobuf format for" - {
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

  "Auto Decoder must convert protobuf format to case class for" - {
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

  "Auto Encoders/Decoders must respect law: encode(i) === encode(decode(encode(i))" - {
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
