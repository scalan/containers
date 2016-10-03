package scalan.containers

import scalan.collections.{CollectionsDslExp, CollectionsDslStd, CollectionsDsl}
import scalan.{ScalanExp, ScalanStd, Scalan}
import scala.reflect.runtime.universe._

trait ArrayContainers { self: ContainersDsl =>

  trait ArrayEnumarable extends ArrayFunctor with Enumerable[Array] {
    def length[A](xs: Rep[Array[A]]): Rep[Int] = {
      implicit val eA = getItemElem(xs)
      xs.length
    }
    def empty[A: Elem] = SArray.empty[A]
    def point[A:Elem](a: Rep[A]) = SArray.singleton(a)
    def ap[A, B: Elem](fa: Rep[Array[A]])(f: Rep[Array[(A) => B]]) = ???
    def plus[A](fa: Rep[Array[A]], fb: Rep[Array[A]]) = ???
  }

  trait ArrayStructured extends ArrayFunctor with Structured[Array]{
    def first[A, B](pairs: Rep[Array[(A, B)]]) = {
      implicit val eA = getItemElem(pairs).eFst
      implicit val eB = getItemElem(pairs).eSnd
      pairs.map(_._1)
    }
    def second[A, B](pairs: Rep[Array[(A, B)]]) = {
      implicit val eA = getItemElem(pairs).eFst
      implicit val eB = getItemElem(pairs).eSnd
      pairs.map(_._2)
    }
  }

  trait ArrayJoinable extends ArrayFunctor with Joinable[Array] {
    def join[A, B](as: Rep[Array[A]], bs: Rep[Array[B]]) = {
      implicit val eA = getItemElem(as)
      implicit val eB = getItemElem(bs)
      as.zip(bs)
    }
  }

  trait ArrayShaped extends ArrayJoinable with Shaped[Int, Array, Array] {
    def eK = element[Int]
    def replicate[A: Elem](shape: Rep[Array[Int]], v: Rep[A]) =
      SArray.replicate(shape.length, v)
    def shape[A](xs: Rep[Array[A]]): Rep[Array[Int]] = {
      implicit val eA = getItemElem(xs)
      SArray.rangeFrom0(xs.length)
    }
  }

  trait ArrayIndexed extends ArrayShaped with Indexed[Int, Array, Array] {
    def get[A](xs: Arr[A], i: Rep[Int]) = {
      implicit val eA = getItemElem(xs)
      xs(i)
    }
    def set[A](xs: Arr[A], index: Rep[Int], value: Rep[A]) = {
      implicit val eA = getItemElem(xs)
      xs.update(index, value)
    }
    def setMany[A](xs: Rep[Array[A]], indexes: Arr[Int], values: Arr[A]): Arr[A] = {
      implicit val eA = getItemElem(xs)
      xs.updateMany(indexes, values)
    }
  }

  trait ArrayTabular extends ArrayStructured with ArrayIndexed
                             with Tabular[Int, Array, Array, Array] {
    def create[A](keys: Rep[Array[Int]], values: Rep[Array[A]]) = ???
    def values[A](xs: Rep[Array[A]]) = xs
  }


}

trait ArrayContainersSeq extends ArrayContainers { self: ContainersDslStd => }
trait ArrayContainersExp extends ArrayContainers { self: ContainersDslExp => }
