package io.github.chikei.spbcodec.tag

private[spbcodec] object TagRestriction {

  /**
   * Explicitly allows primitive types to be tagged
   */
  sealed class NumericTagRestriction[V]

  object NumericTagRestriction {
    implicit val intTagRestriction: NumericTagRestriction[Int]   = new NumericTagRestriction[Int]
    implicit val longTagRestriction: NumericTagRestriction[Long] = new NumericTagRestriction[Long]
  }
}
