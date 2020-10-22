package io.github.chikei.spbcodec.magnolia

import magnolia.{ CaseClass, Magnolia, SealedTrait }

import scala.collection.mutable

trait CustomIndex[A] {
  def index(i: Int): Option[Int]
}

object CustomIndex {
  class MapIndex[A](map: Map[Int, Int]) extends CustomIndex[A] {
    def index(i: Int): Option[Int] = map.get(i)
  }

  final def fromMap[A](map: Map[Int, Int]): CustomIndex[A] = new MapIndex[A](map)

  final def apply[A](implicit instance: CustomIndex[A]): CustomIndex[A] = instance
}

trait Index[A] {
  def index(i: Int): Int
}

object Index {
  val Default = new Index[Any] {
    def index(i: Int): Int = i + 1
  }

  final def apply[A](implicit instance: Index[A]): Index[A] = instance

  implicit def default[A]: Index[A] = Default.asInstanceOf[Index[A]]

  type Typeclass[T] = Index[T]

  // take size and custom mapping to make a map like "custom.getOrElse(index, 'auto-incremental from 1 except custom values')"
  def makeMap(size: Int, custom: Int => Option[Int]): Array[Int] = {
    val used  = mutable.Set.empty[Int]
    val map   = mutable.Map.empty[Int, Int]
    val range = Range(0, size)

    range.foreach { paramIndex =>
      custom(paramIndex).foreach { cIndex =>
        if (!used(cIndex)) {
          map.addOne((paramIndex, cIndex))
          used.addOne(cIndex)
        }
      }
    }

    var pbIndex = 1
    range.foreach { paramIndex =>
      if (!map.contains(paramIndex)) {
        while (used.contains(pbIndex)) pbIndex += 1

        map.addOne((paramIndex, pbIndex))
        pbIndex += 1
      }
    }

    val arr = Array.ofDim[Int](size)
    range.foreach(i => arr.update(i, map(i)))
    arr
  }

  def combine[T](caseClass: CaseClass[Index, T])(implicit customIndex: CustomIndex[T]): Index[T] =
    new Index[T] {
      val indexMapper: Array[Int] = makeMap(caseClass.parameters.size, customIndex.index)

      def index(i: Int): Int = indexMapper(i)
    }

  def dispatch[T](
    sealedTrait: SealedTrait[Index, T]
  )(implicit customIndex: CustomIndex[T]): Index[T] =
    new Index[T] {
      val indexMapper: Array[Int] = makeMap(sealedTrait.subtypes.size, customIndex.index)

      def index(i: Int): Int = indexMapper(i)
    }

  def deriveIndex[T]: Typeclass[T] = macro Magnolia.gen[T]
}
