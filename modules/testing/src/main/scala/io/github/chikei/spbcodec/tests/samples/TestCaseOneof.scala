package io.github.chikei.spbcodec.tests.samples

sealed trait TestCaseOneof

object TestCaseOneof extends TestCase[TestCaseOneof] {
  case class IntValue(value: Int) extends TestCaseOneof
  case class CustomType(
    bigdecimal: BigDecimal,
    bigint: BigInt,
    uuid: java.util.UUID,
    short: Short,
    char: Char
  ) extends TestCaseOneof

  private val value = CustomType(
    bigdecimal = BigDecimal(Double.MaxValue) * BigDecimal(Double.MaxValue),
    bigint = BigInt(Long.MaxValue) * BigInt(Long.MaxValue),
    uuid = java.util.UUID.randomUUID(),
    short = Short.MaxValue,
    char = 'z'
  )

  override val source: TestCaseOneof = value

  override val protobuf: ProtoSerializable = ProtoSerializable(
    Schemas.OneOf
      .newBuilder()
      .setF1(
        Schemas.Custom
          .newBuilder()
          .setBigdecimal(value.bigdecimal.toString())
          .setBigint(value.bigint.toString())
          .addUuid(value.uuid.getMostSignificantBits)
          .addUuid(value.uuid.getLeastSignificantBits)
          .setShort(value.short.toInt)
          .setChar(value.char.toInt)
          .build()
      )
      .build()
  )
}
