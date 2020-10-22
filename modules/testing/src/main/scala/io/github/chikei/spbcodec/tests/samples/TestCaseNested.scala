package io.github.chikei.spbcodec.tests.samples

import io.github.chikei.spbcodec.tests.samples.TestCaseNested.{ InnerNested }

case class TestCaseNested(d: Double, m1: InnerNested, m2: InnerNested, rm: Seq[InnerNested])

object TestCaseNested extends TestCase[TestCaseNested] {
  case class InnerNested(
    bigDecimal: BigDecimal,
    bigInt: BigInt
  )

  override val source: TestCaseNested = TestCaseNested(
    d = 1.0,
    m1 = InnerNested(BigDecimal(Double.MaxValue.toString), BigInt(1)),
    m2 = InnerNested(BigDecimal(Long.MinValue), BigInt(2)),
    rm = Seq(InnerNested(BigDecimal(1), BigInt(3)), InnerNested(BigDecimal(2), BigInt(4)))
  )

  override val protobuf: ProtoSerializable = ProtoSerializable(
    Schemas.Nested
      .newBuilder()
      .setD(source.d)
      .setM1(
        Schemas.Custom
          .newBuilder()
          .setBigdecimal(source.m1.bigDecimal.toString())
          .setBigint(source.m1.bigInt.toString())
          .build()
      )
      .setM2(
        Schemas.Custom
          .newBuilder()
          .setBigdecimal(source.m2.bigDecimal.toString())
          .setBigint(source.m2.bigInt.toString())
          .build()
      )
      .addRm(
        Schemas.Custom
          .newBuilder()
          .setBigdecimal(source.rm(0).bigDecimal.toString())
          .setBigint(source.rm(0).bigInt.toString())
          .build()
      )
      .addRm(
        Schemas.Custom
          .newBuilder()
          .setBigdecimal(source.rm(1).bigDecimal.toString())
          .setBigint(source.rm(1).bigInt.toString())
          .build()
      )
      .build()
  )

}
