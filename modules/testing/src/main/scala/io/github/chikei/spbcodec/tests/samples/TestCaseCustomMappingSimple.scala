package io.github.chikei.spbcodec.tests.samples

import io.github.chikei.spbcodec.tag._
import io.github.chikei.spbcodec.tests.samples.Schemas.Color

case class TestCaseCustomMappingSimple(
  f: Float,
  ui: Int @@ Unsigned,
  c: Colors.Color
)

object TestCaseCustomMappingSimple extends TestCase[TestCaseCustomMappingSimple] {
  override val source: TestCaseCustomMappingSimple = TestCaseCustomMappingSimple(
    f = Float.MaxValue,
    ui = unsigned(Int.MaxValue),
    c = Colors.Green
  )

  override val protobuf: ProtoSerializable = ProtoSerializable(
    Schemas.All
      .newBuilder()
      .setFloatField(source.f)
      .setUint32Field(source.ui)
      .setColorField(Color.GREEN)
      .build()
  )
}
