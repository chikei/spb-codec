package io.github.chikei.spbcodec.tests.samples

import io.github.chikei.spbcodec.tests.samples.Schemas.{ AllValue, Color, IntValue }
import io.github.chikei.spbcodec.tests.samples.TestCaseOptionalFields.TestOptionalCaseObject

case class TestCaseOptionalFields(
  i: Option[Int],
  a: Option[TestCaseAllFields],
  o1: Option[TestOptionalCaseObject.type],
  o2: Option[TestOptionalCaseObject.type]
)

object TestCaseOptionalFields extends TestCase[TestCaseOptionalFields] {
  case object TestOptionalCaseObject

  override val source = TestCaseOptionalFields(
    i = Some(Int.MaxValue),
    a = Some(TestCaseAllFields.source),
    o1 = None,
    o2 = Some(TestOptionalCaseObject)
  )

  override val protobuf: ProtoSerializable = ProtoSerializable {
    val builder = Schemas.Optional
      .newBuilder()
    source.i.foreach(i => builder.setInt(IntValue.newBuilder().setValue(i).build()))
    source.a.foreach(a =>
      builder
        .setAll(
          AllValue
            .newBuilder()
            .setValue(
              Schemas.All
                .newBuilder()
                .setDoubleField(a.d)
                .setFloatField(a.f)
                .setInt32Field(a.i)
                .setInt64Field(a.l)
                .setUint32Field(a.ui)
                .setUint64Field(a.ul)
                .setSint32Field(a.si)
                .setSint64Field(a.sl)
                .setFixed32Field(a.fi)
                .setFixed64Field(a.fl)
                .setSfixed32Field(a.sfi)
                .setSfixed64Field(a.sfl)
                .setBoolField(a.b)
                .setStringField(a.s)
                .setBytesField(a.by)
                .setColorField(Color.GREEN)
                .build()
            )
            .build()
        )
        .build()
    )
    builder.setObj2(
      Schemas.OptionalCaseObject.newBuilder().setO(Schemas.CaseObject.newBuilder().build())
    )
    builder.build()
  }
}
