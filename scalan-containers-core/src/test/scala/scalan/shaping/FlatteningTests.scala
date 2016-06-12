//package scalan
//
//class LambdaShapingTests extends TestingSamples with ScalanEnterpriseDslExp with StagedTesting { scalan =>
//
//  override val prefix = "test-out/Flattening/"
//
//  @Test def samples_tests {
//    doLift(id, "id")
//    doLift(inc, "inc")
//    doLift(tupled, "tupled")
//    doFlatten(mapped, "mapped")
//    doFlatten(zippedMap, "zippedMap")
//    doLift(curred, "curred")
//    doLift(highOrder, "highOrder")
//
//    doLift(inc2, "inc2")
//    doFlatten(mapped2, "mapped2")
//    doLift(inc_times, "inc_times")
//    doFlatten(splitMap, "splitMap")
//    doFlatten(splitMap2, "splitMap2")
//    doFlatten(mapInc3Times, "mapInc3Times")
//    doFlatten(splitMap3, "splitMap3")
//    doFlatten(splitMapMap, "splitMapMap")
//  }
//
//  @Ignore("LiftLambdaMirror can't lift ExpFuncArray")
//  @Test def backPermute_tests {
//    val bp = fun { xs: PA[Float] => is: PA[Int] => xs ->> is }
//    val bpL = doLift(bp, "backPermute")
//    val bpLL = doLift(bpL, "backPermuteLifted")
//  }
//
//  @Test def filter_tests {
//    val f = fun { xs: PA[Float] => xs.filter((x: Rep[Float]) => x > toRep(0).toFloat) }
//    val fF = doFlatten(f, "withFilter")
//    //val bpLL = doLift(bpL, "backPermuteLifted")
//  }
//
//  @Test def high_order_tests {
//    doFlatten(mapInc3Times, "mapInc3Times")
//  }
//
//  @Test def exclude_map_with_scalar_from_flattening {
//    doFlatten(mapScalar, "mapScalar")
//  }
//
//  @Test def exclude_nested_map_with_scalar_from_flattening {
//    doFlatten(mapScalarNested, "mapScalarNested")
//  }
//
//  @Test def exclude_filter_with_scalar_from_flattening {
//    doFlatten(filterScalar, "filterScalar")
//  }
//
//  @Test def exclude_nested_filter_with_scalar_from_flattening {
//    doFlatten(filterScalarNested, "filterScalarNested")
//  }
//
//
//  lazy val noflattenMapMap = fun {(xs: PA[Int]) => xs.flatten.mapBy(inc2).mapBy(inc_times) }
//
//  @Test def noflatten {
//    doFlatten(noflattenMapMap, "noflattenMapMap")
//  }
//
//  @Test def nested_map_tests {
//    val f = fun { example(_: PA[Int]) }
//    doFlatten(f, "f")
//  }
//
//  def example(xs: PA[Int]) =  {
//    def f(y: Rep[Int]) =  xs(y)
//    xs map f
//  }
//
//  lazy val nestedFlattenBug1 = fun {(xss: NA[Int]) =>
//    indexRange(xss.length).map(i => {
//      val xs = xss(i)
//      val ys = xs.filter(x => x > i)
//      val yss = ys.map(s213 =>
//        for (s216 <- xss(s213) if s216 > s213 && xss(s216).contains(i)) yield ()
//      )
//      yss.values.length
//    }).reduce
//  }
//
//  lazy val nestedFlattenBug2 = fun {(in: Rep[(NArray[Int],Int)]) =>
//    val Pair(xss, i) = in
//    val xs = xss(i)
//    val ys = xs.filter(x => x > i)
//    val yss = ys.map(s213 =>
//      for (s216 <- xss(s213) if s216 > s213 && xss(s216).contains(i)) yield ()
//    )
//    yss.values.length
//  }
//
//
//  @Test def nestedFlattenBug1_test {
//    doFlatten(nestedFlattenBug1, "nestedFlattenBug1")
//    doFlatten(nestedFlattenBug2, "nestedFlattenBug2")
//  }
//
//  //------------------------------------------------------------------------------
//  lazy val xss = singleton(1).asNested
//  lazy val nestedFlattenBug3 = fun {(i: Rep[Int]) =>
//    val xs = xss(i)
//    val ys = xs.filter(x => x > i)
//    val yss = ys.map(s213 =>
//      for (s216 <- xss(s213) if s216 > s213 && xss(s216).contains(i)) yield ()
//    )
//    yss.values.length
//  }
//  @Test def nestedFlattenBug3_test {
//    doFlatten(nestedFlattenBug3, "nestedFlattenBug3")
//  }
//
//  //------------------------------------------------------------------------------
//  lazy val filterWithClosure = fun {
//    (i: Rep[Int]) => for (x <- xss(i) if x < i ) yield ()
//  }
//
//  @Test def lifting_of_FilterPA_with_closure_test {
//    val fL = doLift(filterWithClosure, "filterWithClosure")
//  }
//
//  @Test def lifting_of_FilterNA_with_closure_test {
//    val fL = doLift(filterWithClosure, "filterWithClosure")
//    val fLL = doLift(fL, "filterWithClosure_1")
//    val fLLL = doLift(fLL, "filterWithClosure_2")
//  }
//
//  lazy val mapWithClosure = fun {
//    (i: Rep[Int]) => xss(i) map { x => x + i }
//  }
//
//  @Test def lifting_of_MapPA_with_closure_test {
//    val fL = doLift(mapWithClosure, "mapWithClosure")
//  }
//
//  @Test def lifting_of_MapNA_with_closure_test {
//    val fL = doLift(mapWithClosure, "mapWithClosure_1")
//    val fLL = doLift(fL, "mapWithClosure_2")
//    val fLLL = doLift(fLL, "mapWithClosure_3")
//  }
//
//}
