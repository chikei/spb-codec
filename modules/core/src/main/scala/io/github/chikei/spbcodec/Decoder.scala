package io.github.chikei.spbcodec

import java.io.InputStream
import java.nio.ByteBuffer

import com.google.protobuf.CodedInputStream
import io.github.chikei.spbcodec.Decoder.Result
import io.github.chikei.spbcodec.error.DecodingFailure

import scala.annotation.implicitNotFound

/**
 * Interface for all Decoder implementations.
 *
 * Allows to decode protobuf3 serialized message in type `A`. If the message is not compatible with `A`,
 * return a [[io.github.chikei.spbcodec.error.DecodingFailure]].
 */
@implicitNotFound("No Decoder found for type ${A}.")
trait Decoder[A] extends MessageFieldDecoder[A] { self =>

  /**
   * Try to read the value `A` in the CodedInputStream.
   */
  def decode(input: CodedInputStream): Result[A] = {
    val decoder = messageWireDecoder
    decoder.doDecode(input, false).flatMap(_ => this.build(decoder))
  }

  /**
   * Try to read the value `A` from the Array[Byte].
   */
  def decode(input: Array[Byte]): Result[A] = decode(CodedInputStream.newInstance(input))

  /**
   * Try to read the value `A` from the InputStream.
   */
  def decode(input: InputStream): Result[A] = decode(CodedInputStream.newInstance(input))

  /**
   * Try to read the value `A` from the ByteBuffer.
   */
  def decode(input: ByteBuffer): Result[A] = decode(CodedInputStream.newInstance(input))
}

/**
 * Utilities for [[Decoder]]
 */
object Decoder {

  /**
   * Result type of the decoding process.
   */
  type Result[A] = Either[DecodingFailure, A]

  /**
   * Summon a decoder for type `A`.
   */
  def apply[A](implicit decoder: Decoder[A]): Decoder[A] = decoder
}
