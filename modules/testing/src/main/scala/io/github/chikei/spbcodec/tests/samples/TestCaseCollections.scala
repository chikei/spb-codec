package io.github.chikei.spbcodec.tests.samples

import io.github.chikei.spbcodec.tag._

case class TestCaseCollections(
  d: Seq[Double],
  f: List[Float],
  i: collection.immutable.Seq[Int],
  l: Array[Long],
  ui: Vector[Int @@ Unsigned],
  ul: scala.collection.Iterable[Long @@ Unsigned]
)

object TestCaseCollections extends TestCase[TestCaseCollections] {

  override val source: TestCaseCollections = TestCaseCollections(
    d = Seq(1d, 2d),
    f = List(1f, 2f),
    i = scala.collection.immutable.Seq(1, 2),
    l = Array(1L, 2L),
    ui = Seq(unsigned(1), unsigned(2)).toVector,
    ul = Seq(unsigned(1L), unsigned(2L))
  )

  override val protobuf: ProtoSerializable = ProtoSerializable(
    Schemas.Repeated
      .newBuilder()
      .addDoubleField(source.d(0))
      .addDoubleField(source.d(1))
      .addFloatField(source.f(0))
      .addFloatField(source.f(1))
      .addInt32Field(source.i(0))
      .addInt32Field(source.i(1))
      .addInt64Field(source.l(0))
      .addInt64Field(source.l(1))
      .addUint32Field(source.ui(0))
      .addUint32Field(source.ui(1))
      .addUint64Field(source.ul.toList(0))
      .addUint64Field(source.ul.toList(1))
      .build()
  )
}
