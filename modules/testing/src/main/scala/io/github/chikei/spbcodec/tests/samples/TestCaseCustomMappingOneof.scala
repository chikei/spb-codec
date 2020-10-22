package io.github.chikei.spbcodec.tests.samples

import io.github.chikei.spbcodec.tag.{ @@, unsigned, Unsigned }
import io.github.chikei.spbcodec.tests.samples.Schemas.Color

sealed trait TestCaseCustomMappingOneof

object TestCaseCustomMappingOneof extends TestCase[TestCaseCustomMappingOneof] {
  case class CustomType(
    f: Float,
    ui: Int @@ Unsigned,
    c: Colors.Color
  ) extends TestCaseCustomMappingOneof

  val value: CustomType = CustomType(
    f = Float.MaxValue,
    ui = unsigned(Int.MaxValue),
    c = Colors.Green
  )

  override val source: TestCaseCustomMappingOneof = value

  override val protobuf: ProtoSerializable = ProtoSerializable(
    Schemas.OneOf
      .newBuilder()
      .setF3(
        Schemas.All
          .newBuilder()
          .setFloatField(value.f)
          .setUint32Field(value.ui)
          .setColorField(Color.GREEN)
          .build()
      )
      .build()
  )
}
