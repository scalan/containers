package scalan.containers

import java.lang.reflect.Method

import scalan.collections.{CollectionsDslExp, CollectionsDslStd, CollectionsDsl}
import scalan.{ScalanExp, ScalanStd, Scalan}
import scala.reflect.runtime.universe._

trait CollectionContainers { self: ContainersDsl =>

  trait CollectionEnumarable extends CollectionFunctor with Enumerable[Collection] {
    def length[A](xs: Rep[Collection[A]]): Rep[Int] = xs.length
    def empty[A: Elem] = Collection.empty[A]
    def point[A:Elem](a: Rep[A]) = Collection.singleton(a)
    def ap[A, B: Elem](fa: Rep[Collection[A]])(f: Rep[Collection[(A) => B]]) = ???
    def plus[A](fa: Rep[Collection[A]], fb: Rep[Collection[A]]) = ???
  }

  trait CollectionStructured extends CollectionFunctor with Structured[Collection]{
    def first[A, B](pairs: Rep[Collection[(A, B)]]) = pairs.as
    def second[A, B](pairs: Rep[Collection[(A, B)]]) = pairs.bs
  }

  trait CollectionJoinable extends CollectionFunctor with Joinable[Collection] {
    def join[A, B](as: Rep[Collection[A]], bs: Rep[Collection[B]]) = {
      implicit val eB = getItemElem(bs)
      as.zip(bs)
    }
  }

  trait CollectionShaped extends CollectionJoinable with Shaped[Int, Collection, Collection] {
    def eK = element[Int]
    def replicate[A: Elem](shape: Rep[Collection[Int]], v: Rep[A]) = Collection.replicate(shape.length, v)
    def shape[A](xs: Rep[Collection[A]]) = Collection.indexRange(xs.length)
  }

  trait CollectionIndexed extends CollectionShaped with Indexed[Int, Collection, Collection] {
    def get[A](xs: Rep[Collection[A]], i: Rep[Int]) = xs(i)
    def set[A](xs: Coll[A], index: Rep[Int], value: Rep[A]) = xs.update(index, value)
    def setMany[A](xs: Coll[A], indexes: Coll[Int], values: Coll[A]): Coll[A] =
      xs.updateMany(indexes, values)
  }

  trait CollectionTabular extends CollectionStructured with CollectionIndexed
                             with Tabular[Int, Collection, Collection, Collection] {
    def create[A](keys: Rep[Collection[Int]], values: Rep[Collection[A]]) = ???
    def values[A](xs: Rep[Collection[A]]) = xs
  }

  implicit override val collectionContainer =
    new CollectionTabular with CollectionEnumarable {
      def cI = this
      def cC = this
    }

//  implicit val ArrayIsomorphicToCollection: Isomorphic[Array, Collection] = new Isomorphic[Array, Collection] {
//    def cFrom = container[Array]
//    def cTo = collectionContainer
//    def from[A](xs: Rep[Collection[A]]) = xs.arr
//    def to[A](xs: Rep[Array[A]]) = { implicit val eA = cFrom.getItemElem(xs); Collection(xs) }
//  }
//  implicit val CollectionIsomorphicToArray: Isomorphic[Collection, Array] = ArrayIsomorphicToCollection.reverse

}

trait CollectionContainersSeq extends CollectionContainers { self: ContainersDslStd => }
trait CollectionContainersExp extends CollectionsDslExp with CollectionContainers { self: ContainersDslExp =>

  override protected def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]): Elem[_] = receiver.elem match {
    case e: CollectionElem[r,_] => (e.eItem, m.getName) match {
      case (pe: PairElem[a,b], "as") => collectionElement(pe.eFst)
      case (pe: PairElem[a,b], "bs") => collectionElement(pe.eSnd)
      case (_, "apply") => e.eItem
      case _ => super.getResultElem(receiver, m, args)
    }
    case _ => super.getResultElem(receiver, m, args)
  }
}
