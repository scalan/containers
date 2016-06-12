package scalan

import scalan.collections.CollectionsDsl

trait ArraySamples extends ScalanDsl {

  lazy val id = fun {(x: Rep[Int]) => x}
  lazy val inc = fun {(x: Rep[Int]) => x + 1}
  lazy val curred = fun {(x: Rep[Int]) => (y: Rep[Int]) => x + y }
  lazy val tupled = fun {(x: Rep[(Int,Int)]) => x._1 + x._2 }
  lazy val mapped = fun {(xs: Rep[Array[Int]]) => xs.mapBy(inc) }
  lazy val zippedMap = fun {(xs: Arr[Int]) => (xs zip xs).mapBy(tupled) }
  lazy val highOrder = fun {(x: Rep[Int]) => {
    val x1 = x + 1
    fun {(y: Rep[Int]) => y + x + x1 }
  } }

  lazy val inc2 = fun {(x: Rep[Int]) => x + ((1:Rep[Int]) + 1)}
  lazy val mapped2 = fun {(xs: Arr[Int]) => xs.mapBy(inc2) }
  lazy val inc_times = fun {(x: Rep[Int]) => x + ((1:Rep[Int]) + 1) * 2 }

  lazy val splitMap = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2)) }
  lazy val splitMap2 = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc_times), xs.mapBy(inc2)) }
  lazy val mapInc3Times = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc), Pair(xs.mapBy(inc), xs.mapBy(inc))) }
  lazy val splitMap3 = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc), Pair(xs.mapBy(inc2), xs.mapBy(inc_times))) }
  lazy val splitMapMap = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2).mapBy(inc_times)) }

  lazy val scalar = fun { (x: Rep[Int]) => (x + 1) * (x + 2) }
  lazy val mapScalar = fun { (xs: Arr[Int]) => xs.mapBy(scalar) }
  lazy val mapScalarNested = fun { (xs: Arr[Array[Int]]) => xs.mapBy(mapScalar) }

  lazy val filterScalar = fun { (xs: Arr[Int]) => xs.filter(x => x > 0) }
  lazy val filterScalarNested = fun { (xss: Arr[Array[Int]]) => xss.mapBy(filterScalar) }

}

trait CollectionSamples extends CollectionsDsl {

  lazy val id = fun {(x: Rep[Int]) => x}
  lazy val inc = fun {(x: Rep[Int]) => x + 1}
  lazy val curred = fun {(x: Rep[Int]) => (y: Rep[Int]) => x + y }
  lazy val tupled = fun {(x: Rep[(Int,Int)]) => x._1 + x._2 }
  lazy val mapped = fun {(xs: Coll[Int]) => xs.mapBy(inc) }
//  lazy val zippedMap = fun {(xs: Arr[Int]) => (xs zip xs).mapBy(tupled) }
//  lazy val highOrder = fun {(x: Rep[Int]) => {
//    val x1 = x + 1
//    fun {(y: Rep[Int]) => y + x + x1 }
//  } }
//
//  lazy val inc2 = fun {(x: Rep[Int]) => x + ((1:Rep[Int]) + 1)}
//  lazy val mapped2 = fun {(xs: Arr[Int]) => xs.mapBy(inc2) }
//  lazy val inc_times = fun {(x: Rep[Int]) => x + ((1:Rep[Int]) + 1) * 2 }
//
//  lazy val splitMap = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2)) }
//  lazy val splitMap2 = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc_times), xs.mapBy(inc2)) }
//  lazy val mapInc3Times = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc), Pair(xs.mapBy(inc), xs.mapBy(inc))) }
//  lazy val splitMap3 = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc), Pair(xs.mapBy(inc2), xs.mapBy(inc_times))) }
//  lazy val splitMapMap = fun {(xs: Arr[Int]) => Pair(xs.mapBy(inc), xs.mapBy(inc2).mapBy(inc_times)) }
//
//  lazy val scalar = fun { (x: Rep[Int]) => (x + 1) * (x + 2) }
//  lazy val mapScalar = fun { (xs: Arr[Int]) => xs.mapBy(scalar) }
//  lazy val mapScalarNested = fun { (xs: Arr[Array[Int]]) => xs.mapBy(mapScalar) }
//
//  lazy val filterScalar = fun { (xs: Arr[Int]) => xs.filter(x => x > 0) }
//  lazy val filterScalarNested = fun { (xss: Arr[Array[Int]]) => xss.mapBy(filterScalar) }

}
