package scalan.containers

import scalan.util.CollectionUtil._
import scalan.collections.{CollectionsDslExp, CollectionsDslStd, CollectionsDsl}
import scalan.{ScalanExp, ScalanStd, Scalan}
import scala.reflect.runtime.universe._

trait StructuredContainers { self: CollectionsDsl =>

  trait Apply[F[_]] extends Functor[F] {
    def ap[A,B:Elem](fa: Rep[F[A]])(f: Rep[F[(A) => B]]): Rep[F[B]]
  }

  trait Plus[F[_]] extends Cont[F] {
    def plus[A](fa: Rep[F[A]], fb: Rep[F[A]]): Rep[F[A]]
  }

  trait PlusEmpty[F[_]] extends Plus[F] {
    def empty[A:Elem]: Rep[F[A]]
  }

  trait Applicative[F[_]] extends Apply[F] {
    def point[A:Elem](a: Rep[A]): Rep[F[A]]
  }

  trait ApplicativePlus[F[_]] extends Applicative[F] with PlusEmpty[F] {
  }

  trait Joinable[F[_]] extends Functor[F] {
    def join[A,B](as: Rep[F[A]], bs: Rep[F[B]]): Rep[F[(A,B)]]
  }

  trait Structured[F[_]] extends Joinable[F] {
    def first[A,B](pairs: Rep[F[(A,B)]]): Rep[F[A]]
    def second[A,B](pairs: Rep[F[(A,B)]]): Rep[F[B]]
  }

  trait Enumerable[F[_]] extends ApplicativePlus[F] {
    def length[A](xs: Rep[F[A]]): Rep[Int]
  }

  trait Shaped[K, I[_], F[_]] extends Joinable[F] with Functor[F] {
    def eK: Elem[K]
    def cI: Enumerable[I]
    def replicate[A:Elem](shape: Rep[I[K]], v: Rep[A]): Rep[F[A]]
    def shape[A](xs: Rep[F[A]]): Rep[I[K]]
  }

  trait Indexed[K, I[_], F[_]] extends Shaped[K, I, F] {
    def get[A](xs: Rep[F[A]], i: Rep[K]): Rep[A]
    def getMany[A](xs: Rep[F[A]], is: Rep[I[K]]): Rep[F[A]] = {
      implicit val eA = getItemElem[A](xs)
      val units = replicate(is, ())
      map(join(units, xs))(_._2)
    }
    def set[A](cont: Rep[F[A]], index: Rep[K], value: Rep[A]): Rep[F[A]]
    def setMany[A](cont: Rep[F[A]], indexes: Rep[I[K]], values: Rep[I[A]]): Rep[F[A]]
  }

  trait Tabular[K, I[_], C[_], F[_]] extends Structured[F] with Indexed[K, I, F] {
    def cC: Structured[C]
    def create[A](keys: Rep[I[K]], values: Rep[C[A]]): Rep[F[A]]
    def values[A](xs: Rep[F[A]]): Rep[C[A]]
  }

  trait Convertible[From[_], To[_]] { first =>
    def cFrom: Cont[From]
    def cTo: Cont[To]
    def to[A](xs: Rep[From[A]]): Rep[To[A]]

    def >>[To2[_]: Cont](second: Convertible[To,To2]): Convertible[From,To2] = new Convertible[From,To2] {
      def cFrom = first.cFrom
      def cTo = container[To2]
      def to[A](xs: Rep[From[A]]) = second.to(first.to(xs))
    }
  }

  trait Isomorphic[From[_], To[_]] extends Convertible[From,To] { first =>
    def from[A](xs: Rep[To[A]]): Rep[From[A]]

    def >>[To2[_]: Cont](second: Isomorphic[To,To2]): Isomorphic[From,To2] = new Isomorphic[From,To2] {
      def cFrom = first.cFrom
      def cTo = container[To2]
      def to[A](xs: Rep[From[A]]) = second.to(first.to(xs))
      def from[A](ys: Rep[To2[A]]) = first.from(second.from(ys))
    }
    def reverse: Isomorphic[To, From] = new Isomorphic[To,From] {
      def from[A](xs: Rep[From[A]]) = first.to(xs)
      def to[A](xs: Rep[To[A]]) = first.from(xs)
      def cTo = first.cFrom
      def cFrom = first.cTo
    }
  }

  implicit def containerIso[A, From[_], To[_]](implicit eA: Elem[A], iso: Isomorphic[From,To]): Iso[From[A], To[A]] = {
    implicit val eFA = iso.cFrom.lift(eA)
    implicit val eTA = iso.cTo.lift(eA)
    val funTo = fun { (xs: Rep[From[A]]) => iso.to(xs) }
    val convTo = NaturalConverter(funTo)(eA, iso.cFrom, iso.cTo)
    val funFrom = fun { (xs: Rep[To[A]]) => iso.from(xs) }
    val convFrom = NaturalConverter(funFrom)(eA, iso.cTo, iso.cFrom)
    ConverterIso(convTo, convFrom)
  }
}

trait StructuredContainersSeq extends StructuredContainers { self: CollectionsDslStd => }
trait StructuredContainersExp extends StructuredContainers { self: CollectionsDslExp => }
