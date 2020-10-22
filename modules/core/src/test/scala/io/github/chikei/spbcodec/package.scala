package io.github.chikei

import java.io.ByteArrayOutputStream

import com.google.protobuf.{ CodedInputStream, CodedOutputStream, WireFormat }
import io.github.chikei.spbcodec.error.DecodingFailure

package object spbcodec {
  implicit class RichFieldEncoder[T](val enc: FieldEncoder[T]) extends AnyVal {
    def encodeAsBytes(index: Int, a: T): Array[Byte] = {
      val baos = new ByteArrayOutputStream()
      val cos  = CodedOutputStream.newInstance(baos)
      enc.writeToTag(index, a, cos)
      cos.flush()
      baos.toByteArray
    }
  }

  implicit class RichFieldDecoder[T](val dec: FieldDecoder[T]) extends AnyVal {
    def decode(input: Array[Byte], index: Int): Decoder.Result[T] = {
      val cis = CodedInputStream.newInstance(input)
      val tag = cis.readTag()

      val wireDecoder = dec.wireDecoder
      val wireType    = WireFormat.getTagWireType(tag)
      val msgIndex    = WireFormat.getTagFieldNumber(tag)
      if (index == msgIndex) {
        wireDecoder.tryDecodeTag(wireType, cis, false) match {
          case Some(value) => value.flatMap(_ => dec.build(wireDecoder))
          case None        => Left(new DecodingFailure(s"Wrong wire type, read ($wireType)"))
        }
      } else {
        Left(new DecodingFailure(s"Wrong index, expected ($index) read ($msgIndex)"))
      }
    }
  }
}
