package io.github.chikei.spbcodec.error

import com.google.protobuf.InvalidProtocolBufferException
import io.github.chikei.spbcodec.WireDecoder

/**
 * An exception representing a decoding failure associated with a possible cause
 */
sealed class DecodingFailure(message: String, cause: Option[Throwable] = None)
    extends Exception(message, cause.orNull) {

  def withMessage(msg: String): DecodingFailure = new DecodingFailure(msg, cause)
}

case class WrongWireDecoderType(expectedDecoder: String, decoder: WireDecoder)
    extends DecodingFailure(
      s"Expecting $expectedDecoder, seeing ${decoder.getClass.getSimpleName}"
    )
case class WireFieldError(index: Int, cause: DecodingFailure)
    extends DecodingFailure(
      s"Decoding field with number $index failed",
      Some(cause)
    )
case class MissingFieldDecoder(index: Int, fieldName: String)
    extends DecodingFailure(s"Missing WireDecoders for field `$fieldName` with index $index")
case class MissingField(index: Int, fieldName: String)
    extends DecodingFailure(s"Missing field value `$fieldName` with index $index")
case class OneofNotSet(traitName: String)
    extends DecodingFailure(s"Oneof field but never found value ($traitName)")
case class UnknownSubtypeIndex(subtypeIndex: Int, traitName: String)
    extends DecodingFailure(
      s"Unknown subtype index $subtypeIndex with sealed trait $traitName"
    )

case class InternalProtobufError(message: String, cause: Throwable)
    extends DecodingFailure(message, Some(cause))

object DecodingFailure {
  /**
   * Transform a generic Exception into a [[DecodingFailure]].
   */
  def fromThrowable(ex: Throwable, message: String): InternalProtobufError = ex match {
    case err: InvalidProtocolBufferException =>
      val unfinishedMessage = Option(err.getUnfinishedMessage)
        .map(msg => s"\nMessage read: ${msg.toByteArray}")
        .getOrElse("")
      InternalProtobufError(s"$message\n${err.getMessage}\n$unfinishedMessage", ex)

    case _ => InternalProtobufError(ex.getMessage, ex)
  }

  /**
   * Build a [[DecodingFailure]] from a message.
   */
  def apply(message: String): DecodingFailure = new DecodingFailure(message)
}
