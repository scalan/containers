package scalan.containers

import scalan.collections._
import scalan.{ScalanExp, ScalanStd, Scalan}
import scala.reflect.runtime.universe._

trait MMapContainers extends StructuredContainers with CollectionContainers { self: CollectionsDsl =>

  trait MMapContainer[K] extends Cont[({type f[x] = MMap[K,x]})#f] {
    implicit def eK: Elem[K]
    def tag[T](implicit tT: WeakTypeTag[T]) = weakTypeTag[MMap[K,T]]
    def lift[T](implicit eT: Elem[T]) = mMapElement(eK, eT)
    def unlift[A](implicit efa: Elem[MMap[K,A]]) = efa.asInstanceOf[MMapElem[K,A]].eValue
    def getElem[A](fa: Rep[MMap[K,A]]) = fa.selfType1
    def unapply[A](e: Elem[_]) = e match {
      case te: MMapElem[_, _] => Some(te.asElem[MMap[K,A]])
      case _ => None
    }
  }

  trait MMapFunctor[K] extends MMapContainer[K] with Functor[({type f[x] = MMap[K,x]})#f] {
    def map[A:Elem, B:Elem](xs: Rep[MMap[K, A]])(f: (Rep[A]) => Rep[B]) = xs.mapValues(f)
  }

  trait MMapJoinable[K] extends Joinable[({type f[x] = MMap[K,x]})#f] {
    def eK: Elem[K]
    def join[A, B](as: Rep[MMap[K, A]], bs: Rep[MMap[K, B]]) = {
      implicit val _eK = eK
      implicit val eA = getItemElem[A](as)
      implicit val eB = getItemElem[B](bs)
      as.join(bs)
    }
  }

  trait MMapShaped[K] extends MMapJoinable[K] with Shaped[K, Collection, ({type f[x] = MMap[K,x]})#f] {
    def replicate[A: Elem](shape: Rep[Collection[K]], v: Rep[A]) = {
      implicit val _eK = eK
      val vs = SArray.replicate(shape.length, v)
      val kvs = shape.arr.zip(vs)
      MMap.fromArray(kvs)
    }
    def shape[A](xs: Rep[MMap[K, A]]) = {
      implicit val _eK = eK
      implicit val eA = getItemElem[A](xs)
      Collection(xs.keys)
    }
  }

  trait MMapIndexed[K] extends MMapShaped[K] with Indexed[K, Collection, ({type f[x] = MMap[K,x]})#f] {
    def get[A](xs: Rep[MMap[K, A]], i: Rep[K]) = {
      implicit val _eK = eK
      implicit val eA = getItemElem[A](xs)
      xs(i)
    }
    def set[A](xs: Rep[MMap[K, A]], index: Rep[K], value: Rep[A]) = {
      implicit val _eK = eK
      implicit val eA = getItemElem[A](xs)
      xs.update(index, value) | xs
    }
    def setMany[A](xs: Rep[MMap[K, A]], indexes: Rep[Collection[K]], values: Rep[Collection[A]]) = ???
  }

  trait MMapStructured[K] extends MMapFunctor[K] with Structured[({type f[x] = MMap[K,x]})#f]{
    def first[A, B](pairs: Rep[MMap[K, (A, B)]]) = {
      val e = getItemElem(pairs)
      implicit val eA = e.eFst
      implicit val eB = e.eSnd
      pairs.mapValues(_._1)
    }
    def second[A, B](pairs: Rep[MMap[K, (A, B)]]) = {
      val e = getItemElem(pairs)
      implicit val eA = e.eFst
      implicit val eB = e.eSnd
      pairs.mapValues(_._2)
    }
  }

  trait MMapTabular[K] extends MMapStructured[K] with MMapIndexed[K] with Tabular[K, Collection, Collection, ({type f[x] = MMap[K,x]})#f] {
    implicit private def _eK = eK
    def create[A](keys: Rep[Collection[K]], values: Rep[Collection[A]]) = {
      implicit val eA = cC.getItemElem(values)
      mapFromArray(keys.zip(values).arr)
    }
    def values[A](xs: Rep[MMap[K, A]]) = {
      implicit val eA = getItemElem(xs)
      Collection(xs.values)
    }
  }

}

trait MMapContainersSeq extends MMapContainers { self: CollectionsDslStd => }
trait MMapContainersExp extends MMapContainers { self: CollectionsDslExp => }
