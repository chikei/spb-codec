package io.github.chikei.spbcodec.tests

import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.{ EitherValues, OptionValues }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

trait ProtolessSuite
    extends AnyFreeSpec
    with Matchers
    with TypeCheckedTripleEquals
    with EitherValues
    with OptionValues
