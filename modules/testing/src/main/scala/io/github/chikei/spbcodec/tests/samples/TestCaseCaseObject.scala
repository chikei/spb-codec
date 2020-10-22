package io.github.chikei.spbcodec.tests.samples

case object CaseObject

object TestCaseCaseObject extends TestCase[CaseObject.type] {

  override val source: CaseObject.type = CaseObject

  override val protobuf: ProtoSerializable = ProtoSerializable(
    Schemas.CaseObject
      .newBuilder()
      .build()
  )

}
