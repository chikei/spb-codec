package io.github.chikei.spbcodec

import com.google.protobuf.{ ByteString, CodedOutputStream }
import io.github.chikei.spbcodec.Decoder.Result
import io.github.chikei.spbcodec.Encoder.MessageEncoder
import io.github.chikei.spbcodec.WireDecoder.MessageDecoder
import io.github.chikei.spbcodec.error.{ MissingFieldDecoder, WrongWireDecoderType }
import io.github.chikei.spbcodec.tag.{ @@, Fixed, Signed, Unsigned }
import io.github.chikei.spbcodec.tests.ProtolessSuite
import io.github.chikei.spbcodec.tests.samples.Colors.Color
import io.github.chikei.spbcodec.tests.samples.TestCaseNested.InnerNested
import io.github.chikei.spbcodec.tests.samples.{
  TestCaseAllFields,
  TestCaseCustomMappingRepeated,
  TestCaseNested
}

class HandCraftedEncoderDecoderSuite extends ProtolessSuite with EncoderDecoderAssertions {

  // format: off
  implicit val decoderTestCaseAllFields: Decoder[TestCaseAllFields] = new Decoder[TestCaseAllFields] {
    def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder = {
      val decoders = Map(
        1 ->  FieldDecoder[Double].wireDecoder,
        2 ->  FieldDecoder[Float].wireDecoder,
        3 ->  FieldDecoder[Int].wireDecoder,
        4 ->  FieldDecoder[Long].wireDecoder,
        5 ->  FieldDecoder[Int @@ Unsigned].wireDecoder,
        6 ->  FieldDecoder[Long @@ Unsigned].wireDecoder,
        7 ->  FieldDecoder[Int @@ Signed].wireDecoder,
        8 ->  FieldDecoder[Long @@ Signed].wireDecoder,
        9 ->  FieldDecoder[Int @@ Fixed].wireDecoder,
        10 -> FieldDecoder[Long @@ Fixed].wireDecoder,
        11 -> FieldDecoder[Int @@ Signed with Fixed].wireDecoder,
        12 -> FieldDecoder[Long @@ Signed with Fixed].wireDecoder,
        13 -> FieldDecoder[Boolean].wireDecoder,
        14 -> FieldDecoder[String].wireDecoder,
        15 -> FieldDecoder[ByteString].wireDecoder,
        16 -> FieldDecoder[Color].wireDecoder
      )
      new MessageDecoder(decoders)
    }

    def build(wireDecoder: WireDecoder): Result[TestCaseAllFields] = 
      wireDecoder match {
        case decoder: MessageDecoder =>
          val decoders = decoder.value
          for{
            d   <- decoders.get(1 ).toRight(MissingFieldDecoder(1 , "d  ")).flatMap(d => FieldDecoder[Double].build(d))
            f   <- decoders.get(2 ).toRight(MissingFieldDecoder(2 , "f  ")).flatMap(d => FieldDecoder[Float].build(d))
            i   <- decoders.get(3 ).toRight(MissingFieldDecoder(3 , "i  ")).flatMap(d => FieldDecoder[Int].build(d))
            l   <- decoders.get(4 ).toRight(MissingFieldDecoder(4 , "l  ")).flatMap(d => FieldDecoder[Long].build(d))
            ui  <- decoders.get(5 ).toRight(MissingFieldDecoder(5 , "ui ")).flatMap(d => FieldDecoder[Int @@ Unsigned].build(d))
            ul  <- decoders.get(6 ).toRight(MissingFieldDecoder(6 , "ul ")).flatMap(d => FieldDecoder[Long @@ Unsigned].build(d))
            si  <- decoders.get(7 ).toRight(MissingFieldDecoder(7 , "si ")).flatMap(d => FieldDecoder[Int @@ Signed].build(d))
            sl  <- decoders.get(8 ).toRight(MissingFieldDecoder(8 , "sl ")).flatMap(d => FieldDecoder[Long @@ Signed].build(d))
            fi  <- decoders.get(9 ).toRight(MissingFieldDecoder(9 , "fi ")).flatMap(d => FieldDecoder[Int @@ Fixed].build(d))
            fl  <- decoders.get(10).toRight(MissingFieldDecoder(10, "fl ")).flatMap(d => FieldDecoder[Long @@ Fixed].build(d))
            sfi <- decoders.get(11).toRight(MissingFieldDecoder(11, "sfi")).flatMap(d => FieldDecoder[Int @@ Signed with Fixed].build(d))
            sfl <- decoders.get(12).toRight(MissingFieldDecoder(12, "sfl")).flatMap(d => FieldDecoder[Long @@ Signed with Fixed].build(d))
            b   <- decoders.get(13).toRight(MissingFieldDecoder(13, "b  ")).flatMap(d => FieldDecoder[Boolean].build(d))
            s   <- decoders.get(14).toRight(MissingFieldDecoder(14, "s  ")).flatMap(d => FieldDecoder[String].build(d))
            by  <- decoders.get(15).toRight(MissingFieldDecoder(15, "by ")).flatMap(d => FieldDecoder[ByteString].build(d))
            c   <- decoders.get(16).toRight(MissingFieldDecoder(16, "c  ")).flatMap(d => FieldDecoder[Color].build(d))
          } yield TestCaseAllFields(d, f, i, l, ui, ul, si, sl, fi, fl, sfi, sfl, b, s, by, c)
        case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
      }
      }

  implicit val encoderTestCaseAllFields: Encoder[TestCaseAllFields] = new MessageEncoder[TestCaseAllFields] {
    def writeTo(a: TestCaseAllFields, output: CodedOutputStream): Unit = {
      if(!Encoder[Double]                   .isDefault(a.d  )) Encoder[Double]                   .writeToTag(1 , a.d  , output)
      if(!Encoder[Float]                    .isDefault(a.f  )) Encoder[Float]                    .writeToTag(2 , a.f  , output)
      if(!Encoder[Int]                      .isDefault(a.i  )) Encoder[Int]                      .writeToTag(3 , a.i  , output)
      if(!Encoder[Long]                     .isDefault(a.l  )) Encoder[Long]                     .writeToTag(4 , a.l  , output)
      if(!Encoder[Int @@ Unsigned]          .isDefault(a.ui )) Encoder[Int @@ Unsigned]          .writeToTag(5 , a.ui , output)
      if(!Encoder[Long @@ Unsigned]         .isDefault(a.ul )) Encoder[Long @@ Unsigned]         .writeToTag(6 , a.ul , output)
      if(!Encoder[Int @@ Signed]            .isDefault(a.si )) Encoder[Int @@ Signed]            .writeToTag(7 , a.si , output)
      if(!Encoder[Long @@ Signed]           .isDefault(a.sl )) Encoder[Long @@ Signed]           .writeToTag(8 , a.sl , output)
      if(!Encoder[Int @@ Fixed]             .isDefault(a.fi )) Encoder[Int @@ Fixed]             .writeToTag(9 , a.fi , output)
      if(!Encoder[Long @@ Fixed]            .isDefault(a.fl )) Encoder[Long @@ Fixed]            .writeToTag(10, a.fl , output)
      if(!Encoder[Int @@ Signed with Fixed] .isDefault(a.sfi)) Encoder[Int @@ Signed with Fixed] .writeToTag(11, a.sfi, output)
      if(!Encoder[Long @@ Signed with Fixed].isDefault(a.sfl)) Encoder[Long @@ Signed with Fixed].writeToTag(12, a.sfl, output)
      if(!Encoder[Boolean]                  .isDefault(a.b  )) Encoder[Boolean]                  .writeToTag(13, a.b  , output)
      if(!Encoder[String]                   .isDefault(a.s  )) Encoder[String]                   .writeToTag(14, a.s  , output)
      if(!Encoder[ByteString]               .isDefault(a.by )) Encoder[ByteString]               .writeToTag(15, a.by , output)
      if(!Encoder[Color]                    .isDefault(a.c  )) Encoder[Color]                    .writeToTag(16, a.c  , output)
    }

    def size(a: TestCaseAllFields): Int = 
      (if(Encoder[Double]                   .isDefault(a.d  )) 0 else(Encoder[Double]                   .sizeTag(1 , a.d  ))) +
      (if(Encoder[Float]                    .isDefault(a.f  )) 0 else(Encoder[Float]                    .sizeTag(2 , a.f  ))) +
      (if(Encoder[Int]                      .isDefault(a.i  )) 0 else(Encoder[Int]                      .sizeTag(3 , a.i  ))) +
      (if(Encoder[Long]                     .isDefault(a.l  )) 0 else(Encoder[Long]                     .sizeTag(4 , a.l  ))) +
      (if(Encoder[Int @@ Unsigned]          .isDefault(a.ui )) 0 else(Encoder[Int @@ Unsigned]          .sizeTag(5 , a.ui ))) +
      (if(Encoder[Long @@ Unsigned]         .isDefault(a.ul )) 0 else(Encoder[Long @@ Unsigned]         .sizeTag(6 , a.ul ))) +
      (if(Encoder[Int @@ Signed]            .isDefault(a.si )) 0 else(Encoder[Int @@ Signed]            .sizeTag(7 , a.si ))) +
      (if(Encoder[Long @@ Signed]           .isDefault(a.sl )) 0 else(Encoder[Long @@ Signed]           .sizeTag(8 , a.sl ))) +
      (if(Encoder[Int @@ Fixed]             .isDefault(a.fi )) 0 else(Encoder[Int @@ Fixed]             .sizeTag(9 , a.fi ))) +
      (if(Encoder[Long @@ Fixed]            .isDefault(a.fl )) 0 else(Encoder[Long @@ Fixed]            .sizeTag(10, a.fl ))) +
      (if(Encoder[Int @@ Signed with Fixed] .isDefault(a.sfi)) 0 else(Encoder[Int @@ Signed with Fixed] .sizeTag(11, a.sfi))) +
      (if(Encoder[Long @@ Signed with Fixed].isDefault(a.sfl)) 0 else(Encoder[Long @@ Signed with Fixed].sizeTag(12, a.sfl))) +
      (if(Encoder[Boolean]                  .isDefault(a.b  )) 0 else(Encoder[Boolean]                  .sizeTag(13, a.b  ))) +
      (if(Encoder[String]                   .isDefault(a.s  )) 0 else(Encoder[String]                   .sizeTag(14, a.s  ))) +
      (if(Encoder[ByteString]               .isDefault(a.by )) 0 else(Encoder[ByteString]               .sizeTag(15, a.by ))) +
      (if(Encoder[Color]                    .isDefault(a.c  )) 0 else(Encoder[Color]                    .sizeTag(16, a.c  )))
    
    def isDefault(a: TestCaseAllFields): Boolean = 
      Encoder[Double]                   .isDefault(a.d  ) &&
      Encoder[Float]                    .isDefault(a.f  ) &&
      Encoder[Int]                      .isDefault(a.i  ) &&
      Encoder[Long]                     .isDefault(a.l  ) &&
      Encoder[Int @@ Unsigned]          .isDefault(a.ui ) &&
      Encoder[Long @@ Unsigned]         .isDefault(a.ul ) &&
      Encoder[Int @@ Signed]            .isDefault(a.si ) &&
      Encoder[Long @@ Signed]           .isDefault(a.sl ) &&
      Encoder[Int @@ Fixed]             .isDefault(a.fi ) &&
      Encoder[Long @@ Fixed]            .isDefault(a.fl ) &&
      Encoder[Int @@ Signed with Fixed] .isDefault(a.sfi) &&
      Encoder[Long @@ Signed with Fixed].isDefault(a.sfl) &&
      Encoder[Boolean]                  .isDefault(a.b  ) &&
      Encoder[String]                   .isDefault(a.s  ) &&
      Encoder[ByteString]               .isDefault(a.by ) &&
      Encoder[Color]                    .isDefault(a.c  )
      }

  implicit val decoderTestCaseNested: Decoder[TestCaseNested] = {
    implicit val decoderInner: MessageFieldDecoder[InnerNested] = new MessageFieldDecoder[InnerNested] {
      def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder = {
        val decoders = Map(
          1 ->  FieldDecoder[BigDecimal].wireDecoder,
          2 ->  FieldDecoder[BigInt].wireDecoder
        )
        new MessageDecoder(decoders)
    }

      def build(wireDecoder: WireDecoder): Result[InnerNested] = 
        wireDecoder match {
          case decoder: MessageDecoder =>
            val decoders = decoder.value
            for{
              d   <- decoders.get(1 ).toRight(MissingFieldDecoder(1 , "bigDecimal")).flatMap(d => FieldDecoder[BigDecimal].build(d))
              f   <- decoders.get(2 ).toRight(MissingFieldDecoder(2 , "bigInt")).flatMap(d => FieldDecoder[BigInt].build(d))
            } yield InnerNested(d, f)
          case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
        }
          }

    new Decoder[TestCaseNested] {
      def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder = {
        val decoders = Map(
          1 ->  FieldDecoder[Double].wireDecoder,
          2 ->  FieldDecoder[InnerNested].wireDecoder,
          3 ->  FieldDecoder[InnerNested].wireDecoder,
          4 ->  FieldDecoder[Seq[InnerNested]].wireDecoder
        )
        new MessageDecoder(decoders)
      }

      def build(wireDecoder: WireDecoder): Result[TestCaseNested] = 
        wireDecoder match {
          case decoder: MessageDecoder =>
            val decoders = decoder.value
            for{
              d   <- decoders.get(1 ).toRight(MissingFieldDecoder(1 , "d  ")).flatMap(d => FieldDecoder[Double].build(d))
              f   <- decoders.get(2 ).toRight(MissingFieldDecoder(2 , "m1 ")).flatMap(d => FieldDecoder[InnerNested].build(d))
              i   <- decoders.get(3 ).toRight(MissingFieldDecoder(3 , "m2 ")).flatMap(d => FieldDecoder[InnerNested].build(d))
              l   <- decoders.get(4 ).toRight(MissingFieldDecoder(4 , "rm ")).flatMap(d => FieldDecoder[Seq[InnerNested]].build(d))
            } yield TestCaseNested(d, f, i, l)
          case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
        }
          }
  }

  implicit val encoderTestCaseNested: Encoder[TestCaseNested] = {
    implicit val inner = new MessageEncoder[InnerNested] {
      def writeTo(a: InnerNested, output: CodedOutputStream): Unit = {
        if(!Encoder[BigDecimal].isDefault(a.bigDecimal)) Encoder[BigDecimal].writeToTag(1 , a.bigDecimal, output)
        if(!Encoder[BigInt]    .isDefault(a.bigInt    )) Encoder[BigInt]    .writeToTag(2 , a.bigInt    , output)
      }

      def size(a: InnerNested): Int = 
        (if(Encoder[BigDecimal].isDefault(a.bigDecimal)) 0 else(Encoder[BigDecimal].sizeTag(1 , a.bigDecimal))) +
        (if(Encoder[BigInt]    .isDefault(a.bigInt    )) 0 else(Encoder[BigInt]    .sizeTag(2 , a.bigInt    )))
      
      def isDefault(a: InnerNested): Boolean = 
        Encoder[BigDecimal].isDefault(a.bigDecimal) &&
        Encoder[BigInt]    .isDefault(a.bigInt    )
          }

    new MessageEncoder[TestCaseNested] {
      def writeTo(a: TestCaseNested, output: CodedOutputStream): Unit = {
        if(!Encoder[Double]               .isDefault(a.d  )) Encoder[Double]               .writeToTag(1 , a.d , output)
        if(!Encoder[InnerNested]          .isDefault(a.m1 )) Encoder[InnerNested]          .writeToTag(2 , a.m1 , output)
        if(!Encoder[InnerNested]          .isDefault(a.m2 )) Encoder[InnerNested]          .writeToTag(3 , a.m2 , output)
        if(!FieldEncoder[Seq[InnerNested]].isDefault(a.rm )) FieldEncoder[Seq[InnerNested]].writeToTag(4 , a.rm , output)
      }

      def size(a: TestCaseNested): Int = 
        (if(Encoder[Double]               .isDefault(a.d  )) 0 else(Encoder[Double]               .sizeTag(1 , a.d  ))) +
        (if(Encoder[InnerNested]          .isDefault(a.m1 )) 0 else(Encoder[InnerNested]          .sizeTag(2 , a.m1 ))) +
        (if(Encoder[InnerNested]          .isDefault(a.m2 )) 0 else(Encoder[InnerNested]          .sizeTag(3 , a.m2 ))) +
        (if(FieldEncoder[Seq[InnerNested]].isDefault(a.rm )) 0 else(FieldEncoder[Seq[InnerNested]].sizeTag(4 , a.rm )))
      
      def isDefault(a: TestCaseNested): Boolean = 
        Encoder[Double]               .isDefault(a.d  ) &&
        Encoder[InnerNested]          .isDefault(a.m1 ) &&
        Encoder[InnerNested]          .isDefault(a.m2 ) &&
        FieldEncoder[Seq[InnerNested]].isDefault(a.rm )
          }
  }


  implicit val decoderTestCaseCustomMappingRepeated: Decoder[TestCaseCustomMappingRepeated] = new Decoder[TestCaseCustomMappingRepeated] {
    def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder = {
      val decoders = Map(
        3 ->  FieldDecoder[Seq[Int]].wireDecoder,
        8 ->  FieldDecoder[Seq[Long @@ Signed]].wireDecoder,
        13 -> FieldDecoder[Seq[Boolean]].wireDecoder,
        14 -> FieldDecoder[Seq[String]].wireDecoder,
        16 -> FieldDecoder[Seq[Color]].wireDecoder
      )
      new MessageDecoder(decoders)
    }

    def build(wireDecoder: WireDecoder): Result[TestCaseCustomMappingRepeated] = 
      wireDecoder match {
        case decoder: MessageDecoder =>
          val decoders = decoder.value
          for{
            i   <- decoders.get(3 ).toRight(MissingFieldDecoder(3 , "i  ")).flatMap(d => FieldDecoder[Seq[Int]].build(d))
            sl  <- decoders.get(8 ).toRight(MissingFieldDecoder(8 , "sl ")).flatMap(d => FieldDecoder[Seq[Long @@ Signed]].build(d))
            b   <- decoders.get(13).toRight(MissingFieldDecoder(13, "b  ")).flatMap(d => FieldDecoder[Seq[Boolean]].build(d))
            s   <- decoders.get(14).toRight(MissingFieldDecoder(14, "s  ")).flatMap(d => FieldDecoder[Seq[String]].build(d))
            c   <- decoders.get(16).toRight(MissingFieldDecoder(16, "c  ")).flatMap(d => FieldDecoder[Seq[Color]].build(d))
          } yield TestCaseCustomMappingRepeated(i, sl, b, s, c)
        case _ => Left(WrongWireDecoderType("MessageDecoder", wireDecoder))
      }
      }

  implicit val encoderTestCaseCustomMappingRepeated: Encoder[TestCaseCustomMappingRepeated] = new MessageEncoder[TestCaseCustomMappingRepeated] {
    def writeTo(a: TestCaseCustomMappingRepeated, output: CodedOutputStream): Unit = {
      if(!FieldEncoder[Seq[Int]]           .isDefault(a.i )) FieldEncoder[Seq[Int]]           .writeToTag(3 , a.i , output)
      if(!FieldEncoder[Seq[Long @@ Signed]].isDefault(a.sl)) FieldEncoder[Seq[Long @@ Signed]].writeToTag(8 , a.sl, output)
      if(!FieldEncoder[Seq[Boolean]]       .isDefault(a.b )) FieldEncoder[Seq[Boolean]]       .writeToTag(13, a.b , output)
      if(!FieldEncoder[Seq[String]]        .isDefault(a.s )) FieldEncoder[Seq[String]]        .writeToTag(14, a.s , output)
      if(!FieldEncoder[Seq[Color]]         .isDefault(a.c )) FieldEncoder[Seq[Color]]         .writeToTag(16, a.c , output)
    }

    def size(a: TestCaseCustomMappingRepeated): Int = 
      (if(FieldEncoder[Seq[Int]]           .isDefault(a.i )) 0 else(FieldEncoder[Seq[Int]]           .sizeTag(3 , a.i ))) +
      (if(FieldEncoder[Seq[Long @@ Signed]].isDefault(a.sl)) 0 else(FieldEncoder[Seq[Long @@ Signed]].sizeTag(8 , a.sl))) +
      (if(FieldEncoder[Seq[Boolean]]       .isDefault(a.b )) 0 else(FieldEncoder[Seq[Boolean]]       .sizeTag(13, a.b ))) +
      (if(FieldEncoder[Seq[String]]        .isDefault(a.s )) 0 else(FieldEncoder[Seq[String]]        .sizeTag(14, a.s ))) +
      (if(FieldEncoder[Seq[Color]]         .isDefault(a.c )) 0 else(FieldEncoder[Seq[Color]]         .sizeTag(16, a.c )))
    
    def isDefault(a: TestCaseCustomMappingRepeated): Boolean = 
      FieldEncoder[Seq[Int]]           .isDefault(a.i ) &&
      FieldEncoder[Seq[Long @@ Signed]].isDefault(a.sl) &&
      FieldEncoder[Seq[Boolean]]       .isDefault(a.b ) &&
      FieldEncoder[Seq[String]]        .isDefault(a.s ) &&
      FieldEncoder[Seq[Color]]         .isDefault(a.c )
      }
  // format: on
  "Hand crafted Encoders must convert case class to protobuf format for" - {
    "protobuf native fields type" in {
      testEncoding(TestCaseAllFields)
    }
    "nested fields" in {
      testEncoding(TestCaseNested)
    }
    "repeated fields with custom mapping" in {
      testEncoding(TestCaseCustomMappingRepeated)
    }
  }

  "Hand crafted Decoders must convert protobuf format to case class for" - {
    "protobuf native fields type" in {
      testDecoding(TestCaseAllFields)
    }
    "nested fields" in {
      testDecoding(TestCaseNested)
    }
    "repeated fields with custom mapping" in {
      testDecoding(TestCaseCustomMappingRepeated)
    }
  }

  "Hand crafted Encoders/Decoders must respect law: encode(i) === encode(decode(encode(i))" - {
    "protobuf native fields type" in {
      testFullCycle(TestCaseAllFields)
    }
    "nested fields" in {
      testFullCycle(TestCaseNested)
    }
    "repeated fields with custom mapping" in {
      testFullCycle(TestCaseCustomMappingRepeated)
    }
  }

}
