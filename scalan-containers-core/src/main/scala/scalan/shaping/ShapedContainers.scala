package scalan.shaping

import scala.reflect
import scala.reflect.runtime.universe._
import scalan.Scalan
import scalan.collections.CollectionsDsl
import scalan.containers.{ContainersDsl, MMapContainers, CollectionContainers}
import scalan.collections._

trait ShapedContainers extends CollectionContainers with MMapContainers { self: ContainersDsl =>

  trait Vectorizable[K, I[_], C[_], F[_]] extends Tabular[K, I, C, F] {
    def beginLambda[A](x: Rep[F[A]]): Vectorizable[K, I, C, F]
    def endLambda: Vectorizable[K, I, C, F]

    def replicate[A:Elem](v: Rep[A]): Rep[F[A]]

    def liftFunc[A:Elem, B:Elem, Env:Elem](e: Rep[F[Env]], f: Rep[((K, A))=>B]): Rep[F[A=>B]]
    def liftUnOp[A,R](op: UnOp[A,R], xs: Rep[F[A]]): Rep[F[R]]
    def liftBinOp[A,R](op: BinOp[A,R], xs: Rep[F[A]], ys: Rep[F[A]]): Rep[F[R]]
  }

  class CollectionVectorizable(len: Rep[Int], parent: List[Vectorizable[Int, Collection, Collection, Collection]])
      extends CollectionTabular with Vectorizable[Int, Collection, Collection, Collection] {
    def cI = collectionContainer//.asInstanceOf[Enumerable[Collection]]
    def cC = collectionContainer//.asInstanceOf[Structured[Collection]]

    def beginLambda[A](x: Rep[Collection[A]]) = {
      val newShaped = new CollectionVectorizable(x.length, this :: parent)
      newShaped
    }
    def endLambda = parent.head

    def replicate[A:Elem](v: Rep[A]) = Collection.replicate(len, v)

    def liftFunc[A:Elem, B:Elem, Env:Elem](e: Coll[Env], f: Rep[((Int, A)) => B]) = {
      FuncCollection(e, f)
    }

    def liftUnOp[A, R](op: UnOp[A, R], xs: Rep[Collection[A]]) =
      xs.map(x => applyUnOp(op, x))(op.eResult)

    def liftBinOp[A, R](op: BinOp[A, R], xs: Rep[Collection[A]], ys: Rep[Collection[A]]) = {
      implicit val eA = ys.selfType.eItem
      xs.zip(ys).map(p => applyBinOp(op, p._1, p._2))(op.eResult)
    }
  }


  class MapVectorizable[K:Elem,E:Elem](m: Rep[MMap[K,E]], parent: List[Vectorizable[K, Collection, Collection,  ({type f[x] = MMap[K,x]})#f]])
      extends MMapTabular[K] with Vectorizable[K, Collection, Collection, ({type f[x] = MMap[K,x]})#f] {
    def eK = element[K]
    def cI = collectionContainer//.asInstanceOf[Enumerable[Collection]]
    def cC = collectionContainer//.asInstanceOf[Structured[Collection]]
    def beginLambda[A](x: Rep[MMap[K,A]]) = {
      implicit val eA = getItemElem(x)
      val newShaped = new MapVectorizable(x, this :: parent)
      newShaped
    }

    def endLambda = parent.head

    def replicate[A:Elem](v: Rep[A]) = {
      val len = m.keys.length
      MMap.fromArray(m.keys.zip(SArray.replicate(len, v)))
    }

    def liftFunc[A:Elem, B:Elem, Env:Elem](e: MM[K,Env], f: Rep[((K, A)) => B]) = ???

    def liftUnOp[A, R](op: UnOp[A, R], xs: Rep[MMap[K, A]]) = {
      implicit val eA = getItemElem(xs)
      xs.mapValues(x => applyUnOp(op, x))(op.eResult)
    }

    def liftBinOp[A, R](op: BinOp[A, R], xs: Rep[MMap[K,A]], ys: Rep[MMap[K,A]]) = {
      implicit val eA = getItemElem(ys)
      xs.join(ys).mapValues(p => applyBinOp(op, p._1, p._2))(op.eResult)
    }
  }

}

