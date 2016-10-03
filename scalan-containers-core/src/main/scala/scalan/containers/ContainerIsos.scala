package scalan.containers

import scalan.util.CollectionUtil._
import scalan.collections.{CollectionsDslExp, CollectionsDslStd, CollectionsDsl}
import scalan.{ScalanExp, ScalanStd, Scalan}
import scala.reflect.runtime.universe._

trait ContainerIsos { self: ContainersDsl =>

  def getRowElem[F[_], S <: Struct](eSchema: Elem[S])(implicit cF: Structured[F]) = (eSchema match {
    case se: StructElem[S] @unchecked =>
      val resFields = se.fields.map { case (fn, cF(eFA))  =>
        (fn, cF.unlift(eFA.asElem[F[Any]]))
      }
      structElement(resFields)
    case _ => !!!(s"Schema struct should contain only ColumnElem fields: $eSchema")
  }).asElem[Struct]

  case class StructuredIso[F[_], V, Val <: Struct, Schema <: Struct](eTo: Elem[Schema])(implicit val cF: Structured[F]) extends IsoUR[F[V], Schema] {
    val pairifyIso = PairifyIso[V,Val](getRowElem[F,Schema](eTo).asElem[Val])
    val eFrom = cF.lift(pairifyIso.eFrom)

    def from(columns: Rep[Schema]) =  {
      val res = foldRight[(String,Elem[_]), Rep[F[Any]]](eTo.fields)
        { case (fn,_) => columns.getUntyped(fn).asRep[F[Any]] }
        { case ((fn,fe), col: Rep[F[c]] @unchecked) =>
          implicit val eC = cF.getItemElem(col)
          columns.getUntyped(fn) match { case aCol: Rep[F[a]] @unchecked => cF.join(aCol, col).asRep[F[Any]] }
        }
      res.asRep[F[V]]
    }

    def to(fa: Rep[F[V]]) = {
      val names = eTo.fieldNames
      val fields = new scala.collection.mutable.ArrayBuffer[(String, Rep[_])]
      var i = 0
      var currCol: Rep[F[_]] = fa
      while (i + 1 < names.length) {
        val fn = names(i)
        val pairCol = currCol.asRep[F[(Any,Any)]]
        val col = cF.first(pairCol)
        currCol = cF.second(pairCol)
        fields += (fn -> col)
        i = i + 1
      }
      fields += (names.last -> currCol)
      struct(fields).asRep[Schema]
    }

    override def equals(other: Any) = other match {
      case iso: StructuredIso[_,_,_,_] =>
        (this eq iso) || (eFrom == iso.eFrom && eTo == iso.eTo)
      case _ => false
    }
    lazy val selfType = new ConcreteIsoElem[F[V], Schema, StructuredIso[F, V, Val, Schema]](eFrom, eTo).asElem[IsoUR[F[V], Schema]]
  }

  case class ContainerOfStructsIso[F[_], Val <: Struct, ValSchema <: Struct]
        (eTo: Elem[ValSchema])(implicit val cF: Structured[F])
    extends IsoUR[F[Val], ValSchema] {
    implicit val eVal = getRowElem[F,ValSchema](eTo).asElem[Val]
    val cFIso = StructuredIso[F,Any,Val,ValSchema](eTo)
    val eFrom = cF.lift(eVal)

    def from(columns: Rep[ValSchema]) =  {
      val zipped = cFIso.from(columns)
      implicit val eAny = cF.getItemElem(zipped)
//      implicit val eStruct = eVal.asElem[Struct]
      val res = cF.map(zipped)(cFIso.pairifyIso.to(_))
      res.asRep[F[Val]]
    }

    def to(fa: Rep[F[Val]]) = {
      implicit val eAny = cFIso.pairifyIso.eFrom
      cFIso.to(cF.map(fa)(cFIso.pairifyIso.from(_)))
    }

    override def equals(other: Any) = other match {
      case iso: ContainerOfStructsIso[_,_,_] =>
        (this eq iso) || (eFrom == iso.eFrom && eTo == iso.eTo)
      case _ => false
    }
    lazy val selfType = new ConcreteIsoElem[F[Val], ValSchema, ContainerOfStructsIso[F, Val, ValSchema]](eFrom, eTo).asElem[IsoUR[F[Val], ValSchema]]
  }

  case class IsomorphicContainersIso[From[_], To[_], FromSchema <: Struct, ToSchema <: Struct]
        (eFrom: Elem[FromSchema])
        (implicit val cIso: Isomorphic[From,To]) extends IsoUR[FromSchema, ToSchema] {
    implicit val cFrom = cIso.cFrom
    implicit val cTo = cIso.cTo
    val eTo: Elem[ToSchema] = {
      val fieldsTo = eFrom.fields.map { case (fn, fe: Elem[From[a]] @unchecked) =>
        assert(cFrom.unapply(fe).isDefined, s"Struct should contain containers of type $cFrom but has $fn:$fe")
        val eVal = cFrom.unlift(fe)
        (fn, cTo.lift(eVal))
      }
      structElement(fieldsTo).asElem[ToSchema]
    }

    def from(ys: Rep[ToSchema]) =  {
      val fieldsFrom = eTo.fields.map { case (fn, fe: Elem[To[a]] @unchecked) =>
        val fa = cIso.from(ys.getUntyped(fn).asRep[To[a]])
        (fn, fa)
      }
      struct(fieldsFrom).asRep[FromSchema]
    }

    def to(xs: Rep[FromSchema]) = {
      val fieldsTo = eFrom.fields.map { case (fn, fe: Elem[From[a]] @unchecked) =>
        val ta = cIso.to(xs.getUntyped(fn).asRep[From[a]])
        (fn, ta)
      }
      struct(fieldsTo).asRep[ToSchema]
    }

    override def equals(other: Any) = other match {
      case iso: IsomorphicContainersIso[_,_,_,_] =>
        (this eq iso) || (eFrom == iso.eFrom && eTo == iso.eTo)
      case _ => false
    }
    lazy val selfType = new ConcreteIsoElem[FromSchema, ToSchema,
                              IsomorphicContainersIso[From, To, FromSchema, ToSchema]](eFrom, eTo)
                                .asElem[IsoUR[FromSchema, ToSchema]]
  }

  case class TabularIso[I[_], C[_], F[_], Key, Val, Schema <: Struct]
        (keys: Rep[I[Key]], eTo: Elem[Schema])
        (implicit cF: Tabular[Key, I, C, F]) extends IsoUR[F[Val], Schema] {
    val eFrom = cF.lift(tuplifyStruct(getRowElem(eTo)(cF.cC))).asElem[F[Val]]

    def from(columns: Rep[Schema]) =  {
      implicit val cI = cF.cI
      implicit val cC = cF.cC
      implicit val eKey = cI.getItemElem(keys)
      val unpackedSchema = getRowElem[C, Schema](eTo)
      val res = foldRight[(String,Elem[_]), Rep[F[Any]]](eTo.fields)
        { case (fn,_) =>
          implicit val eCol = unpackedSchema(fn).asElem[Any]
          cF.create(keys, columns.getUntyped(fn).asRep[C[Any]]) }
        { case ((fn,fe), tab: Rep[F[c]] @unchecked) =>
          implicit val eC = cF.getItemElem(tab)
          columns.getUntyped(fn) match {
            case aCol: Rep[C[a]] @unchecked =>
              implicit val eA = unpackedSchema(fn).asElem[a]
              val aTab = cF.create(keys, aCol)
              cF.join(aTab, tab).asRep[F[Any]]
          }
        }
      res.asRep[F[Val]]
    }

    def to(fa: Rep[F[Val]]) = {
      val names = eTo.fieldNames
      val fields = new scala.collection.mutable.ArrayBuffer[(String, Rep[_])]
      var currFA = fa.asRep[F[Any]]
      if (names.length == 1) {
        fields += (names(0) -> cF.values(currFA))
      }
      else {
        var i = 0
        while (i + 2 < names.length) {
          val fn = names(i)
          val pairs = currFA.asRep[F[(Any, Any)]]
          val tab = cF.values(cF.first(pairs))
          currFA = cF.second(pairs)
          fields += (fn -> tab)
          i = i + 1
        }
        val pairs = currFA.asRep[F[(Any, Any)]]
        fields += (names(i) -> cF.values(cF.first(pairs)))
        fields += (names(i + 1) -> cF.values(cF.second(pairs)))
      }
      struct(fields).asRep[Schema]
    }

    override def equals(other: Any) = other match {
      case iso: TabularIso[_,_,_,_,_,_] =>
        (this eq iso) || (eFrom == iso.eFrom && eTo == iso.eTo && keys == iso.keys)
      case _ => false
    }
    lazy val selfType = new ConcreteIsoElem[F[Val], Schema, TabularIso[I, C, F, Key, Val, Schema]](eFrom, eTo).asElem[IsoUR[F[Val], Schema]]
  }


}

trait ContainerIsosSeq extends ContainerIsos { self: ContainersDslStd => }
trait ContainerIsosExp extends ContainerIsos { self: ContainersDslExp => }
