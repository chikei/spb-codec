package io.github.chikei.spbcodec.tests.instances

import org.scalactic.Equality

import io.github.chikei.spbcodec.tests.samples.TestCaseCollections

trait EqualityInstances {

  // Array are compared by instance instead of by value by default. We need to override this behavior with
  // a custom equality for TestCaseCollections
  implicit val equalityTestCaseCollections = new Equality[TestCaseCollections] {
    override def areEqual(a: TestCaseCollections, b: Any): Boolean =
      b match {
        case o: TestCaseCollections =>
          a.l.toList == o.l.toList && // override array comparison...
            a.d == o.d && a.f == o.f && a.i == o.i && a.ul == o.ul &&
            a.ui == o.ui
        case _ => false
      }
  }

}
