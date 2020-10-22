package io.github.chikei.spbcodec

trait MessageFieldDecoder[A] extends FieldDecoder[A] {
  def messageWireDecoder: WireDecoder.DelimitedMessageWireDecoder

  def wireDecoder: WireDecoder = messageWireDecoder
}
