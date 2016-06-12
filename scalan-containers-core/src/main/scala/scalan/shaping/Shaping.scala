package scalan.shaping

import scalan.collections.{CollectionsDslStd, CollectionsDsl}
import scalan.compilation.{GraphVizExport, Compiler}
import scalan.containers.{ContainersDslExp, ContainersDslStd, ContainersDsl}
import scalan.staged.{Expressions, ProgramGraphs}
import scalan._
import scalan.common.Lazy

trait Shaping extends ShapedContainers { self: ContainersDsl =>
}
trait ShapingDsl extends CollectionsDsl with Shaping { self: ContainersDsl => }
trait ShapingDslStd extends CollectionsDslStd with Shaping { self: ContainersDslStd => }

trait ShapingDslExp extends ShapingDsl { self: ContainersDslExp =>

  def mirrorApply[A,B](lam: Lambda[A, B], s: Exp[A], subst: Map[Exp[_], Exp[_]]): Exp[B] = {
    val body = lam.scheduleSyms
    val (t, _) = DefaultMirror.mirrorSymbols(new MapTransformer(subst + (lam.x -> s)), NoRewriting, lam, body)
    t(lam.y).asRep[B]
  }


  class VectContext[K, I[_], C[_], F[_]](private val lifted: Map[Exp[_], Rep[F[_]]], private val subst: Map[Exp[_], Exp[_]])
                         (implicit val eK: Elem[K], val cF: Vectorizable[K, I, C, F], val ops: TransformerOps[VectContext[K, I, C, F]])
      extends MapTransformer(subst) {
    def getLifted[A](k: Rep[A]) = lifted.get(k) //.asInstanceOf[Option[Rep[F[A]]]]

    def +^[A](kv: (Rep[A], Rep[F[A]])): VectContext[K, I, C, F] =
      new VectContext(lifted + kv, subst + kv)

    def beginLambda[A](x: Rep[F[A]]): VectContext[K, I, C, F] = {
      val newShaped = cF.beginLambda(x)
      val newOps = VectContext.ops(eK, newShaped)
      new VectContext(lifted, subst)(eK, newShaped, newOps)
    }
    def endLambda: VectContext[K, I, C, F] = {
      val newShaped = cF.endLambda
      val newOps = VectContext.ops(eK, newShaped)
      new VectContext(lifted, subst)(eK, newShaped, newOps)
    }

    def replicate[A:Elem](v: Rep[A]): Rep[F[A]] = cF.replicate(v)

    def mkIndexedLambda[A, B](f: Rep[A => B], fvs: List[Rep[_]], fvsL: List[Rep[F[_]]]): Rep[((K,A)) => B] = {
      type RF[A] = Rep[F[A]]
      implicit val eA = f.elem.eDom
      implicit val eB = f.elem.eRange
      val fI = fun { (in: Rep[(K, A)]) =>
        val Pair(i, x) = in
        val subst = (fvs, fvsL).zipped.map { case (fv, fvL: RF[a]) => (fv, cF.get[a](fvL, i)) }.toMap
        mirrorApply(f.getLambda, x, subst)
      }
      fI
    }

    def liftFunc[A:Elem,B:Elem, Env:Elem](e: Rep[F[Env]], f: Rep[((K,A))=>B]): Rep[F[A=>B]] = cF.liftFunc(e, f)

    def replicateFunc[A: Elem, B: Elem](f: Rep[A => B]): Rep[F[A => B]] = {
      val fI = mkIndexedLambda(f, Nil, Nil)
      val env = cF.replicate(())
      liftFunc(env, fI)
    }

    override def toString =
      if (lifted.isEmpty && subst.isEmpty) {
        "VectContext.Empty"
      } else {
        s"VectContext($lifted, $subst)"
      }
  }

  object VectContext {
    def empty[K, I[_], C[_], F[_]](implicit eK: Elem[K], cF: Vectorizable[K, I, C, F], ops: TransformerOps[VectContext[K, I, C, F]]) = new VectContext[K, I, C, F](Map(), Map())

    implicit def ops[K, I[_], C[_], F[_]](implicit eK: Elem[K], cF: Vectorizable[K, I, C, F]): TransformerOps[VectContext[K, I, C, F]] = new TransformerOps[VectContext[K, I, C, F]] {
      def empty = VectContext.empty(eK, cF, this)

      def add[A](t: VectContext[K, I, C, F], kv: (Rep[A], Rep[A])): VectContext[K, I, C, F] =
        new VectContext(t.lifted, t.subst + kv)(eK, t.cF, t.ops)

      override def merge(ctx1: VectContext[K, I, C, F], ctx2: VectContext[K, I, C, F]): VectContext[K, I, C, F] =
        new VectContext(ctx1.lifted ++ ctx2.lifted, ctx1.subst ++ ctx2.subst)(eK, ctx1.cF, ctx1.ops)
    }
  }

//  private def unpairMany(s: Exp[_]): List[Exp[_]] = s match {
//    case Def(Tup(x, y)) => unpairMany(x) ::: unpairMany(y)
//    case _ => List(s)
//  }
//
  private def zipMany[K, I[_], C[_], F[_]](env: List[Rep[F[_]]])(implicit eK: Elem[K], cF: Vectorizable[K, I, C, F]): Rep[F[_]] = {
    type RF[A] = Rep[F[A]]
    env.asInstanceOf[List[RF[Any]]].reduceRight[RF[Any]]{
      case (as: RF[a], bs: RF[b]) => cF.join[a,b](as, bs).asRep[F[Any]]
    }
  }

//  private def unzipMany[K, I[_], C[_], F[_]](arr: Rep[F[_]], list: List[Exp[_]]): List[F[_]] = {
//    val arrP = arr.asPA[(Any, Any)]
//    list match {
//      case x :: Nil => List(arr)
//      case x :: y :: Nil => List(arrP.as, arrP.bs)
//      case x :: xs =>
//        arrP.as :: unzipMany(arrP.bs, xs)
//      case _ => !!!(s"Expected PairArray but was $arr")
//    }
//  }
//
//  private def zipLiftedMany(nestedArrays: List[NA[_]]): NA[_] = {
//    val zippedValues = zipMany(nestedArrays map { _.values })
//    zippedValues.nestBy(nestedArrays.head.segments)
//  }

  // It mirrors ONLY specified lambda.
  // All the external symbols are either reused directly or indirectly (i.e. by replication)
  class ShapeLambdaMirror[K, I[_], C[_], F[_], LA, LB](lambdaSym: Exp[LA => LB])
                                       (implicit val eK: Elem[K], val cF: Vectorizable[K, I, C, F], ops: TransformerOps[VectContext[K, I, C, F]])
      extends Mirror[VectContext[K, I, C, F]] {
    type RF[A] = Rep[F[A]]
    type NF[A] = Rep[F[F[A]]]
    val lambda = lambdaSym match {
      case Def(l: Lambda[LA, LB] @unchecked) => l
      case Def(x) => !!!(s"Definition of ${lambdaSym.toStringWithType} should be lambda: $x")
      case _ => !!!(s"${lambdaSym.toStringWithType} must have a definition")
    }
//    def getLiftedVar(ctx: VectContext[F]) = ctx.getLifted(lambda.x).get
//    def getLength(ctx: VectContext[F]) = getLiftedVar(ctx).length //t(lambda.x).asRep[PArray[Any]].length

    override def mirrorSymbols(t0: VectContext[K, I, C, F], rewriter: Rewriter, g: AstGraph, nodes: Seq[Exp[_]]) = {
      val monoidLambdas = nodes.flatMap {
        case Def(MethodCall(_, _, args, _)) =>
          args.collect { case m: RepMonoid[_] => m.append.asInstanceOf[Exp[_]] }
        case _ => Nil
      }.toSet
      val filteredNodes = nodes.filter(!monoidLambdas.contains(_))
      super.mirrorSymbols(t0, rewriter, g, filteredNodes)
    }

    override protected def mirrorVar[A](t: VectContext[K, I, C, F], rewriter: Rewriter, v: Exp[A]): (VectContext[K, I, C, F], Exp[_]) =
      v == lambda.x match {
        case true =>
          implicit val eA = v.elem
          val vL = fresh[F[A]]
          ((t +^ (v -> vL)).beginLambda(vL), vL)
        case _ =>
          super.mirrorVar(t, rewriter, v)
      }

    override protected def getMirroredLambdaSym[A, B](sym: Exp[A => B]) = {
      val eFunc = sym.elem
      val eFA = cF.lift(eFunc.eDom)
      val eFB = cF.lift(eFunc.eRange)
      val funcE = funcElement(eFA, eFB)
      fresh[F[A]=>F[B]](Lazy(funcE))
    }

    override protected def mirrorLambda[A, B](t: VectContext[K, I, C, F], rewriter: Rewriter, f: Exp[A => B], fNode: Lambda[A, B]) = {
      if (f == lambdaSym) {
        // applying mirror to the lambda that we want to lift
        val (t1, fL: RF[LA => LB]@unchecked) = super.mirrorLambda(t, rewriter, f, fNode)
        ((t1 +^ (lambdaSym -> fL)).endLambda, fL)
      } else {
        // applying mirror to a lambda in the lifted body
        ensureLiftedClosure(t, f, f) { (t1, fvs, fvsL) =>
          implicit val eA = f.elem.eDom
          implicit val eB = f.elem.eRange
          val fI = t1.mkIndexedLambda(f, fvs, fvsL)
          val env = fvsL match {
            case Nil =>
              t1.replicate(())
            case _ =>
              zipMany(fvsL)(eK, cF)
          }
          env match {
            case env: Exp[F[e]] @unchecked =>
              implicit val eEnv = cF.unlift(env.elem)
              val fL: RF[A => B] = t1.liftFunc(env, fI)
              (t1 +^ (f -> fL), fL)
          }
        }
      }
    }

    override def apply[A](t: VectContext[K, I, C, F], rw: Rewriter, s: Exp[A], d: Def[A]): (VectContext[K, I, C, F], Exp[_]) = lambda.isLocalDef(s) match {
      case true =>
        s match {
          case Def(d) => d match {
            case Const(_) => replicateSym(t, s)
//            case VarPM(vs: PM[a, b]) => t.getLifted(vs) match {
//              case Some(vsL) => (t +^ (s.asRep[PMap[a, b]] -> vsL), vsL)
//              case None => !!!(s"Don't know how to vectorize VarPM node whose var is not lifted: $s -> $d")
//            }
            case First(pair: Exp[(A, b)] @unchecked) => t.getLifted(pair) match {
              case Some(pairL) =>
                val pairs = pairL.asRep[F[(A,b)]]
                val sL = cF.first(pairs)
                (t +^ (s -> sL), sL)
              case None =>
                freeVarLifter(t, s)
            }
            case Second(pair: Exp[(b, A) @unchecked]) => t.getLifted(pair) match {
              case Some(pairL) =>
                val pairs = pairL.asRep[F[(b,A)]]
                val sL = cF.second(pairs)
                (t +^ (s -> sL), sL)
              case None =>
                freeVarLifter(t, s)
            }
//          case PairArrayCompanionMethods.first(pairs) => t.getLifted(pairs) match {
//            case Some(pairsL: NA[(a, b)] @unchecked) =>
//              implicit val eA = pairsL.elem.eItem.eItem.eFst
//              val sL = mapLifted(pairsL)(pairs => pairs.as).asRF[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case PairArrayCompanionMethods.second(pairs) => t.getLifted(pairs) match {
//            case Some(pairsL: NA[(a, b)] @unchecked) =>
//              implicit val eB = pairsL.elem.eItem.eItem.eSnd
//              val sL = mapLifted(pairsL)(pairs => pairs.bs).asRF[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case NestedArrayCompanionMethods.createSegmentsFromLens(lens) => t.getLifted(lens) match {
//            case Some(lensL) =>
//              //val sL = mapLifted(lensL)(NestedArray.createSegmentsFromLens(_)).asRF[A]
//              //val Pair(scanned, total) = lensL.values.scan //NestedArray.createSegmentsFromLens(lensL.values)
//              val scanned = NestedArray.createSegmentsFromLens(lensL.values).as
//              val segLens = lensL.segLengths
//              val lens1 = segLens <<- (0, 0)
//              val subs = (scanned ->> lens1).expandBy(lensL.segments)
//              val offs = scanned -^ subs.values
//              val sL = (offs zip lensL.values).nestBy(lensL.segments).asRF[A]
//
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case PArrayMethods.length(arr) => t.getLifted(arr) match {
//            case Some(arrL) =>
//              val sL = arrL.segLengths.asRF[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case NestedArrayCompanionMethods.totalLength(segs) =>
//            ensureLifted(t, s, segs) { (t1, segsL) =>
//              val sL = segsL.values.bs.nestBy(segsL.segments).reduceSeg.asRF[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayCompanionMethods.replicate(len, v: Rep[a]) =>
//            ensureLifted(t, s, len, v) { (t1, lenL, vL) =>
//              implicit val eA = v.elem
//              val sL0: NA[a] = vL.expandByLengths(lenL)
//              val sL = sL0.asRF[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayMethods.expandBy(vs: RF[a] @unchecked, segd) =>
//            ensureLifted(t, s, segd, vs.asRF[a]) { (t1, segdL, vsL) =>
//              implicit val eA = vsL.elem.eItem.eItem
//              val zipped = segdL.zipLifted(vsL)
//              val sL0 = mapLifted(zipped) { pairs =>
//                val (segd1, vs) = pairs.unzip
//                vs.expandByLengths(segd1.bs)
//              }
//              val sL = sL0.asRF[A]
//              (t1 +^ (s -> sL), sL)
//              /*
//              //DON'T DO this optimisation:
//              implicit val eA = vsL.elem.eItem.eItem
//              val flatSegs = segdL.values
//              val flatVs = vsL.values
//              val resFlat = flatVs.expandBy(flatSegs)
//              val sL0 = resFlat.nestBy(vsL.segments)
//              val sL = sL0.asRF[A]
//              (t1 +^ (s -> sL), sL)
//              * Because in lifted segments each rows begins with zero offset.
//              * */
//
//            }
//          case PArrayCompanionMethods.indexRange(len) =>
//            ensureLifted(t, s, len) { (t1, lenL) =>
//              val sL0: NA[Int] = NestedArray.indexRanges(NestedArray.createSegmentsFromLens(lenL))
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case NestedArrayCompanionMethods.indexRanges(segments) =>
//            ensureLifted(t, s, segments) { (t1, segsL) =>
//              val sL0: NA[_] = mapLifted(segsL) { segs =>
//                val lens = segs.bs
//                NestedArray.indexRanges(NestedArray.createSegmentsFromLens(lens))
//              }
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayMethods.indexOf(xs, v: Exp[a]) =>
//            ensureLifted(t, s, xs.asPA[a], v) { (t1, xsL, vL) =>
//              val sL0: PA[Int] = NestedArray.indexOfSeg(xsL, vL)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case NestedArrayCompanionMethods.indexOfSeg(xss, vs: PA[a]) =>
//            ensureLifted(t, s, xss.asNA[a], vs.asPA[a]) { (t1, xssL, vsL) =>
//              val zipped = xssL.zipLifted(vsL)
//              val sL0 = mapLifted(zipped) { pairs =>
//                val (xss, vs) = pairs.unzip
//                NestedArray.indexOfSeg(xss, vs)
//              }
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayMethods.contains1(xs, v: Exp[a], sorted) =>
//            ensureLifted(t, s, xs.asPA[a], v) { (t1, xsL, vL) =>
//              val sL0: PA[Boolean] = NestedArray.containsLifted(xsL, vL, sorted)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s ->   sL), sL)
//            }
//          case NestedArrayCompanionMethods.containsLifted(xss, vs: PA[a], sorted) =>
//            ensureLifted(t, s, xss.asNA[a], vs.asPA[a]) { (t1, xssL, vsL) =>
//              val zipped = xssL.zipLifted(vsL)
//              val sL0 = mapLifted(zipped) { pairs =>
//                val (xss, vs) = pairs.unzip
//                NestedArray.containsLifted(xss, vs, sorted)
//              }
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayMethods.intersect1(xs: PA[a], ys, sorted) =>
//            ensureLifted(t, s, xs.asPA[a], ys.asPA[a]) { (t1, xsL, ysL) =>
//              val sL0: NA[a] = NestedArray.intersectLifted(xsL, ysL, sorted)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case NestedArrayCompanionMethods.intersectLifted(xss: NA[a], yss, sorted) =>
//            ensureLifted(t, s, xss.asNA[a], yss.asNA[a]) { (t1, xssL, yssL) =>
//              val zipped = xssL.zipLifted(yssL)
//              val sL0 = mapLifted(zipped) { pairs =>
//                val (xss, yss) = pairs.unzip
//                NestedArray.intersectLifted(xss, yss, sorted)
//              }
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case Tup(a: Exp[ta], b: Exp[tb]) =>
//            ensureLifted(t, s, a, b) { (t1, aL, bL) =>
//              aL.elem.eItem match {
//                //case e: PArrayElem[_, _, _] =>
//                //  val sL: PA[_] = zipLifted(aL, bL)
//                //  (t1 +^ (s -> sL), sL)
//                case _ =>
//                  implicit val eB = bL.elem.eItem
//                  val sL0 = aL zip bL
//                  val sL = sL0.asPA[A]
//                  (t1 +^ (s -> sL), sL)
//              }
//            }
//          case PArrayMethods.apply(arr, i) => (t.getLifted(arr.asPA[A]), t.getLifted(i)) match {
//            case (None, Some(iL)) =>
//              val sL = arr.asPA[A] ->> iL
//              (t +^ (s -> sL), sL)
//            case (Some(arrL), Some(iL)) =>
//              val sL: PA[A] = NestedArray.applyLifted(arrL, iL)
//              (t +^ (s -> sL), sL)
//            case (Some(arrL), None) =>
//              val (t1, iL) = replicateSym(t, i)
//              val sL: PA[A] = NestedArray.applyLifted(arrL, iL)
//              (t1 +^ (s -> sL), sL)
//            case (None, None) => freeVarLifter(t, s)
//          }
//          case NestedArrayCompanionMethods.backPermuteLifted(xss: NA[a] @unchecked, iss) =>
//            ensureLifted(t, s, xss.asNA[a], iss) { (t1, xssL, issL) =>
//              val newXss: NA[a] = NestedArray.backPermuteLifted(xssL.values, issL.values)
//              val sL0: NA[_] = newXss.nestBy(issL.segments)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayMethods.backPermuteSeg(xs: PA[a] @unchecked, iss) =>
//            ensureLifted(t, s, xs.asPA[a], iss) { (t1, xsL, issL) =>
//              implicit val eA = xsL.elem.eItem.eItem
//              val iss = issL.values
//              val segLens = iss.segLengths
//              val nSegLens = segLens.nestBy(issL.segments)
//              val lens = nSegLens.reduceSeg
//              val newIss = iss.values
//              val newXss: NA[a] = NestedArray.backPermuteLifted(xsL, newIss.nestByLengths(lens))
//              //val sL0: NA[_] = newXss.nestBy(isss.segments)
//              val tmp = newXss.values.nestBy(iss.segments)
//              val sL0: NA[_] = newXss.nestBy(issL.segments)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayMethods.backPermute(xs: PA[a] @unchecked, is) => (t.getLifted(xs.asPA[a]), t.getLifted(is)) match {
//            case (Some(xsL), Some(isL)) =>
//              val sL0: NA[a] = NestedArray.backPermuteLifted(xsL, isL)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            case (None, Some(isL)) =>
//              val sL0: NA[a] = xs.backPermuteSeg(isL)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            case (Some(xsL), None) =>
//              implicit val eA = xsL.elem.eItem.eItem
//              val lens = replicate(xsL.length, is.length)
//              val ofss = xsL.segOffsets.expandByLengths(lens).values
//              val isL = replicateSeg(xsL.length, is)
//              val res = xsL.values ->> (isL +^ ofss)
//              val sL0: NA[a] = res.nestByLengths(lens)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            case _ =>
//              freeVarLifter(t, s)
//          }
//          case PArrayMethods.scan(xs, m: RepMonoid[a]) => t.getLifted(xs) match {
//            case Some(xsL) =>
//              val sL0 = xsL.asNA[a].scanSeg(m)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
          case ApplyUnOp(op: UnOp[a, A @unchecked], arg) =>
            ensureLifted(t, s, arg) { (t1, xs) =>
              implicit val eRes = op.eResult
              val sL: Rep[F[A]] = cF.liftUnOp(op, xs)
              (t1 +^ (s -> sL), sL)
            }
          case ApplyBinOp(op: BinOp[a, A @unchecked], lhs, rhs) =>
            ensureLifted(t, s, lhs, rhs) { (t1, xs, ys) =>
              implicit val eRes = op.eResult
              val sL: Rep[F[A]] = cF.liftBinOp(op, xs, ys)
              (t1 +^ (s -> sL), sL)
            }
//          case op: NumericRand[a] =>
//            ensureLifted(t, s, op.bound) { (t1, xs) =>
//              implicit val eRes = op.elem
//              val sL0: PA[a] = PArray.random(xs)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayCompanionMethods.liftUnOp(op: UnOp[a, r], xs) =>
//            ensureLifted(t, s, xs.asPA[a]) { (t1, xsL) =>
//              implicit val eR: Elem[r] = op.eResult
//              val sL0: NA[r] = mapLifted(xsL)(xss => PArray.liftUnOp(op, xss))
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayCompanionMethods.liftBinOp(op: BinOp[a, r], xs, ys) =>
//            ensureLifted(t, s, xs.asPA[a], ys.asPA[a]) { (t1, xsL, ysL) =>
//              val zipped = zipLifted(xsL, ysL)
//              implicit val eR: Elem[r] = op.eResult
//              val sL0: NA[r] = mapLifted(zipped) { pairs =>
//                val (as, bs) = pairs.unzip
//                PArray.liftBinOp(op, as, bs)
//              }
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case IfThenElse(cond, x, y) =>
//            ensureLiftedAll(t, s, cond, x, y) {
//              case (t1, Seq(condL: PA[Boolean] @unchecked, xL: PA[A] @unchecked, yL: PA[A] @unchecked)) =>
//                implicit val eA = xL.elem.eItem
//                val sL = xL.flagCombine(yL, condL)
//                (t1 +^ (s -> sL), sL)
//              case _ => !!!
//            }
//          case PArrayMethods.flagCombine(ts, fs, flags) =>
//            ensureLiftedAll(t, s, ts, fs, flags) {
//              case (t1, Seq(tsL: NA[a] @unchecked, fsL, flagsL: NA[Boolean] @unchecked)) =>
//                implicit val eA = tsL.elem.eItem.eItem
//                val fsL1 = fsL.asNA[a]
//                val sL0 = mapLifted(tsL.zipLifted(fsL1).zipLifted(flagsL)) { xs =>
//                  val (ys, flags) = xs.unzip
//                  val (ts, fs) = ys.unzip
//                  ts.flagCombine(fs, flags)
//                }
//                val sL = sL0.asPA[A]
//                (t1 +^ (s -> sL), sL)
//              case _ => !!!
//            }
//          case PArrayMethods.filterByMask(arr, flags) =>
//            ensureLiftedAll(t, s, arr, flags) {
//              case (t1, Seq(arrL: NA[a] @unchecked, flagsL: NA[Boolean] @unchecked)) =>
//                implicit val eA = arrL.elem.eItem.eItem
//                val vs = arrL.values.filterByMask(flagsL.values)
//                val mask: PA[Int] = PArray.liftUnOp(BooleanToInt, flagsL.values)
//                val lens = mask.nestBy(flagsL.segments).reduceSeg
//                val sL0 = vs.nestByLengths(lens)
//                val sL = sL0.asPA[A]
//                (t1 +^ (s -> sL), sL)
//              case _ => !!!
//            }
//          case PArrayMethods.mapBy(arr, f: Rep[Function1[a, b]] @unchecked) =>
//            ensureLifted(t, s, arr, f) { (t, arrL, fL) =>
//              val arrN = arrL.asNA[a]
//              val Def(d: ExpFuncArray[_, _, _]) = fL
//              val fI = d.indexedFunc1
//              implicit val eA: Elem[a] = f.elem.eDom
//              implicit val eB: Elem[b] = f.elem.eRange
//              val sL0: NA[b] = NestedArray.deepMap[a, b, a, b](arrN, fI)(eA, eB, eA, eB)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            }
//          case NestedArrayCompanionMethods.deepMap(arr: NA[a], f: Rep[Function1[(Int, c), d]] @unchecked, _, eB: Elem[b], _, _) =>
//            ensureLifted(t, s, arr, f) { (t, arrL, fL) =>
//              val arrN2 = arrL.asPA[NArray[a]]
//              val Def(d: ExpFuncArray[_, _, _]) = fL
//              val fI = d.indexedFunc1
//              val eRes = parrayElement(eB)
//              val sL0: PA[NArray[b]] =
//                NestedArray.deepMap[PArray[a], PArray[b], (Int, c), d](arrN2, fI)(arrN2.elem.eItem.eItem, eRes, f.elem.eDom, f.elem.eRange)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            }
//          case PArrayMethods.filterBy(arr, f: Rep[Function1[a, Boolean]] @unchecked) =>
//            ensureLifted(t, s, arr, f.asRep[a => Boolean]) { (t, arrL, fL) =>
//              val arrN = arrL.asNA[a]
//              val Def(d: ExpFuncArray[_, _, _]) = fL
//              val fI = d.indexedFunc1
//              val eA = f.elem.eDom
//              // was FilterNA(arrN, fI)(eA, d.eB) Check types!
//              val sL0: NA[a] = NestedArray.deepFilter[a, a](arrN, fI)(eA, eA)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            }
//          case NestedArrayCompanionMethods.deepFilter(arr: NA[a] @unchecked, f: Rep[Function1[(Int, b), Boolean]] @unchecked, _, _) =>
//            ensureLifted(t, s, arr, f) { (t, arrL, fL) =>
//              val arrN2 = arrL.asPA[NArray[a]]
//              val Def(d: ExpFuncArray[_, _, _]) = fL //TODO Check types!
//              val fI = d.indexedFunc1
//              val sL0: PA[NArray[a]] =
//                NestedArray.deepFilter[PArray[a], (Int, b)](arrN2, fI)(arrN2.elem.eItem.eItem, f.elem.eDom)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            }
//          //          case PArrayMethods.mapBy(arr, f) =>
//          //            ensureLifted(t, s, arr) { (t, arrL) =>
//          //            ensureLiftedClosure(t, s, f) { (t, fvs, fvsL) =>
//          //              val arrN = arrL.asNA[Any]
//          //              val sL0: NA[Any] = f.asFunc[Any,Any,NA[Any]] { implicit eA => eB => f =>
//          //                val fI = mkIndexedLambda(f, fvs, fvsL)(eA,eB)
//          //                MapNA(arrN, fI)(eA,eB,eA,eB)
//          //              }
//          //              (t +^ (s -> sL), sL)
//          //            }}
//          //          case map@MapNA(arr, f) =>  // PA[A].mapBy[B]
//          //            ensureLifted(t, s, arr) { (t, arrL) =>
//          //            ensureLiftedClosure(t, s, f) { (t, fvs, fvsL) =>
//          //              val arrN2 = arrL.asPA[NArray[Any]]
//          //              val sL0: NA[_] = f.asFunc[Any,Any,NA[PArray[Any]]] { eC => eB => f =>
//          //                val fI = mkIndexedLambda(f, fvs, fvsL)(eC,eB)
//          //                implicit val eAny = parrayElement(map.eB)//: Elem[Any] = arrN2.elem.ea.ea.ea
//          //                val d = MapNA(arrN2, fI)(arrN2.elem.ea.ea, eAny, eC, map.eD)
//          //                d
//          //              }
//          //              (t +^ (s -> sL), sL)
//          //            }}
//          //          case PArrayMethods.filterBy(arr, f) =>
//          //            ensureLifted(t, s, arr) { (t, arrL) =>
//          //            ensureLiftedClosure(t, s, f) { (t, fvs, fvsL) =>
//          //              val arrN = arrL.asNA[Any]
//          //              val sL0: NA[_] = f.asFunc[Any,Boolean,NA[Any]] { implicit eA => eB => f =>
//          //                val fI = mkIndexedLambda(f, fvs, fvsL)(eA,eB)
//          //                FilterNA(arrN, fI)
//          //              }
//          //              (t +^ (s -> sL), sL)
//          //            }}
//          //          case FilterNA(arr, f) =>
//          //            ensureLifted(t, s, arr) { (t, arrL) =>
//          //            ensureLiftedClosure(t, s, f) { (t, fvs, fvsL) =>
//          //              val arrN = arrL.asPA[NArray[Any]]
//          //              implicit val eAny = arrN.elem.ea.ea.ea
//          //              val sL0: NA[_] = f.asFunc[Any,Boolean,NA[PArray[Any]]] { eB => eA => f =>
//          //                val fI = mkIndexedLambda(f, fvs, fvsL)(eB,eA)
//          //                FilterNA(arrN, fI)(parrayElement(eAny),eB)
//          //              }
//          //              (t +^ (s -> sL), sL)
//          //            }}
//          case PArrayMethods.reduce(arr, m: RepMonoid[a]) => t.getLifted(arr) match {
//            case Some(arrL) =>
//              //implicit val eAny = rAny.eA
//              val sL: PA[A] = arrL.asNA[a].reduceSeg(m).asPA[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case NestedArrayCompanionMethods.reduceSeg(nested, m: RepMonoid[a]) => t.getLifted(nested.asNA[a]) match {
//            case Some(nestedL) =>
//              val sL0 = mapLifted(nestedL)(nest => nest.reduceSeg(m))
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case NestedArrayCompanionMethods.reduceSeg1(vs, segs, m: RepMonoid[a]) =>
//            ensureLifted(t, s, vs.asPA[a], segs) { (t1, vsL, segsL) =>
//              // Something wrong here!!!
//              /*val tmp = vsN.zipLifted(segsN)
//              val sL0: NA[Any] = mapLifted(tmp/*vsN.zipLifted(segsN)*/)(ps => {
//                val (vs, segs) = unzip(ps)
//                vs.reduceSeg(segs)(m)
//              })*/
//              val segLengths = segsL.values.bs
//              val newSegs = NestedArray.createSegmentsFromLens(segLengths)
//              val sL0 = vsL.values.reduceSeg1(newSegs)(m).nestBy(segsL.segments)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            }
//          case ExpUnitArray(len) =>
//            ensureLifted(t, s, len) { (t1, lenL) =>
//              val vals: PA[Unit] = ExpUnitArray(lenL.reduce)
//              val sL0 = vals.nestByLengths(lenL)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case ExpPairArray(as: PA[a], bs: PA[b]) =>
//            ensureLifted(t, s, as.asPA[a], bs.asPA[b]) { (t1, asL, bsL) =>
//              val sL0 = zipLifted(asL, bsL)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case ExpNestedArray(segs, vs: PA[a]) =>
//            ensureLifted(t, s, vs.asPA[a], segs) { (t1, vsL, segsL) =>
//              implicit val eA = vsL.elem.eItem.eItem
//              val lens = segsL.segLengths
//              val vals = vsL.values.nestByLengths(segsL.values.bs)
//              val sL0 = vals.nestByLengths(lens)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//          case PArrayMethods.flatten(nested) => t.getLifted(nested) match {
//            case Some(sL0) =>
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case Apply(f, x: Rep[a]) =>
//            ensureLifted(t, s, f.asRep[a => A], x) { (t1, fL, xL) =>
//              implicit val ea = x.elem
//              implicit val eA = s.elem
//              val sL: PA[A] = PArray.liftApply(fL, xL)
//              (t +^ (s -> sL), sL)
//            }
////          case d @ Apply(f, x: Rep[a]) => {
////            val f1 = f.asRep[a => A]
////            // TODO there is a problem with fL's type
////            val (t1, fL, xL) = (t.getLifted(f1), t.getLifted(x)) match {
////              case (Some(fL), Some(xL)) =>
////                (t, fL, xL)
////              case (None, Some(xL)) =>
////                // TODO why isn't t used here?
////                val (_, fL) = liftLambdaSym(VectContext[F].Empty, rw, f1)
////                (t, fL, xL)
////              case (Some(fL), None) =>
////                val (t1, xL) = freeVarLifter(t, x)
////                (t1, fL, xL)
////              case _ =>
////                !!!(s"No lifted argument: $s -> $d")
////            }
////            implicit val ea = x.elem
////            implicit val eA = s.elem
////            val sL = applyByMirroringInContext(t, fL.asRep[PArray[a] => PArray[A]], xL)
////            (t +^ (s -> sL), sL)
////          }
//          case NestedArrayCompanionMethods.values(nested: NA[a]) => t.getLifted(nested.asNA[a]) match {
//            case Some(nestedL) =>
//              implicit val eA = nestedL.elem.eItem.eItem.eItem
//              val vals = nestedL.values
//              val sLens = vals.segLengths
//              val lens = (sLens.nestBy(nestedL.segments)).reduceSeg
//              val sL0 = vals.values.nestByLengths(lens)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case NestedArrayCompanionMethods.segments(nested: NA[a]) => t.getLifted(nested.asNA[a]) match {
//            case Some(nestedL) =>
//              implicit val eA = nestedL.elem.eItem.eItem.eItem
//              val sL0 = nestedL.values.segments.nestBy(nestedL.segments)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            case None =>
//              freeVarLifter(t, s)
//          }
//          case UnpackableDef(repr, iso: Iso[a, A] @unchecked) =>
//            val args = unpairMany(repr)
//            ensureLiftedAll(t, s, args: _*) {
//              case (t1, argsL) =>
//                val arrL = zipMany(argsL).asPA[a]
//                val sL = mkView(arrL)(iso)
//                (t1 +^ (s -> sL), sL)
//              case _ => !!!
//            }
//          case ExpViewPArray(arr, iso: Iso[a, b]) =>
//            ensureLifted(t, s, arr.asPA[a]) { (t1, arrL) =>
//              implicit val eA = arrL.elem.eItem.eItem
//              val ePB = parrayElement(iso.eTo.asInstanceOf[Elem[b]])
//              val sL0: NA[b] = mapLifted(arrL) { arr =>
//                reifyObject(ExpViewPArray(arr, iso))
//              }
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            }
//          case d @ UnpackArrayView(view: PA[a]) =>
//            // TODO problem with types
//            ensureLifted(t, s, view.asPA[a]) { (t1, viewL) =>
//              val iso = d.iso.asInstanceOf[Iso[A, a]]
//              val eA = parrayElement(iso.eFrom)
//              val sL0 = mapLifted(viewL) { view =>
//                reifyObject(UnpackArrayView(view)(iso))
//              }
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            }
//          case NestedArrayCompanionMethods.applyLifted(arrN, ind) => ensureLiftedAll(t, s, arrN, ind) {
//            case (t1, Seq(arrNL: PA[NArray[a]] @unchecked, indL: NA[Int] @unchecked)) => {
//              val arrN2 = arrNL.asPA[NArray[a]]
//              val sL0 = NestedArray.applyLifted(arrN2.values, indL.values).nestBy(indL.segments)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//            case _ => !!!
//          }
//          /*(t.getLifted(arrN), t.getLifted(ind)) match {
//            case (Some(arrNL: PA[NArray[a]] @unchecked), Some(indL)) => {
//              val arrN2 = arrNL.asPA[NArray[a]]
//              implicit val eA = arrN2.elem.eItem.eItem.eItem
//              val sL0 = IndexLiftedPA(arrN2.values, indL.values).nestBy(indL.segments)
//              val sL = sL0.asPA[A]
//              (t +^ (s -> sL), sL)
//            }
//            //TODO
//            /*case (None, Some(indL)) =>
//              case (Some(arrNL), None) =>
//              case (None, None) =>
//              freeVarLifter(t, s)*/
//            case _ => !!!(s"No lifted argument: $s -> $d")
//          }   */
//
//          case PArrayMethods.slice(arr, start, len) => ensureLiftedAll(t, s, arr, start, len) {
//            case (t1, Seq(arrL: NA[a] @unchecked, startL: PA[Int] @unchecked, lenL: PA[Int] @unchecked)) =>
//              implicit val eA = arrL.elem.eItem.eItem
//              val sL0 = NestedArray.sliceLifted(arrL, startL, lenL)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            case _ => !!!
//          }
//          case NestedArrayCompanionMethods.sliceLifted(xss, starts, lens) => ensureLiftedAll(t, s, xss, starts, lens) {
//            case (t1, Seq(xssL: PA[NArray[a]] @unchecked, startsL: NA[Int] @unchecked, lensL: NA[Int] @unchecked)) =>
//              implicit val eA = xssL.elem.eItem.eItem.eItem
//              val sL0 = NestedArray.sliceLifted(xssL.values, startsL.values, lensL.values).nestBy(xssL.segments)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            case _ => !!!
//          }
//
//          case PArrayMethods.permuteDefault(xs, is, vs, inPlace) => ensureLiftedAll(t, s, xs, is, vs) {
//            case (t1, Seq(xsL: NA[a] @unchecked, isL: NA[Int] @unchecked, vsL)) => {
//              val vsL1 = vsL.asNA[a]
//              val offs = (xsL.segOffsets).expandBy(isL.segments)
//              val newIssFlat = isL.values +^ offs.values
//              val flatRes = (xsL.values).permuteDefault(newIssFlat, vsL1.values, inPlace)
//              val sL0 = flatRes.nestBy(xsL.segments)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//            case _ => !!!
//          }
//          case PArrayMethods.permuteReduce(xs, is, vs, m) => ensureLiftedAll(t, s, xs, is, vs) {
//            case (t1, Seq(xsL: NA[a] @unchecked, isL: NA[Int] @unchecked, vsL1)) => {
//              val vsL = vsL1.asPA[PArray[a]]
//              val offs = (xsL.segOffsets).expandBy(isL.segments)
//              val newIssFlat = isL.values +^ offs.values
//              val flatRes = (xsL.values).permuteReduce(newIssFlat, vsL.values)(m.asInstanceOf[RepMonoid[a]])
//              val sL0 = flatRes.nestBy(xsL.segments)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//            case _ => !!!
//          }
//          case PArrayMethods.permuteReduceNested(xs, is, vss, m) => ensureLiftedAll(t, s, xs, is, vss) {
//            case (t1, Seq(xsL: NA[a] @unchecked, isL: NA[Int] @unchecked, vssL)) => {
//              val vssL1 = vssL.asPA[NArray[a]]
//              val offs = (xsL.segOffsets).expandBy(isL.segments)
//              val newIssFlat = isL.values +^ offs.values
//              val flatRes = (xsL.values).permuteReduceNested(newIssFlat, vssL1.values)(m.asInstanceOf[RepMonoid[a]])
//              val sL0 = flatRes.nestBy(xsL.segments)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//            case _ => !!!
//          }
//          case PArrayMethods.permuteReduceCondNested(xs, is, flags, vss, m) => ensureLiftedAll(t, s, xs, is, flags, vss) {
//            case (t1, Seq(xsL: NA[a] @unchecked, isL: NA[Int] @unchecked, flagsL: NA[Boolean] @unchecked, vssL)) => {
//              val vssL1 = vssL.asPA[NArray[a]]
//              val offs = (xsL.segOffsets).expandBy(isL.segments)
//              val newIssFlat = isL.values +^ offs.values
//              val flatRes = (xsL.values).permuteReduceCondNested(newIssFlat, flagsL.values, vssL1.values)(m.asInstanceOf[RepMonoid[a]])
//              val sL0 = flatRes.nestBy(xsL.segments)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//            case _ => !!!
//          }
//          case PArrayCompanionMethods.replicateSeg(count, seg: PA[a] @unchecked) => ensureLifted(t, s, count, seg) { (t1, countL, segL) =>
//            val lengths = countL *^ segL.segLengths
//            val sL0 = segL.expandByLengths(countL).values.values
//            val sL1 = sL0.nestByLengths(lengths)
//            val sL = sL1.asPA[A]
//            (t1 +^ (s -> sL), sL)
//          }
//          case d @ PMapDifference(m1: PM[k, v], m2) =>
//            ensureLifted(t, s, m1, m2) { (t1, m1L, m2L) =>
//              val m1s = m1L.asPA[PMap[k, v]]
//              val m2s = m2L.asPA[PMap[k, v]]
//              implicit val ePMap: PMapElem[k, v] = m2.elem.asInstanceOf[PMapElem[k, v]]
//              val zipped = m1s.zip(m2s)
//              val sL0 = zipped.map { pair =>
//                val m1 = pair._1
//                val m2 = pair._2
//                resolvePMap(m1)(ePMap.eKey, ePMap.eValue).difference(m2)
//              }(ePMap)
//              val sL = sL0.asPA[A]
//              (t1 +^ (s -> sL), sL)
//            }
//            case PairArrayCompanionMethods.innerJoin(xs: PA[(k,a)] @unchecked, ys: PA[(_,b)] @unchecked, f @ Def(lam: Lambda[_,r]), ordK, eR) => ensureLiftedAll(t,s, xs ,ys) {
//              case (t1, Seq(xsL1, ysL1)) => {
//                val ysL = ysL1.asNA[(k,b)]
//                val xsL = xsL1.asNA[(k,a)]
//
//                val res = PairArray.innerJoinNested(xsL, ysL, f.asRep[((a, b)) => r])(ordK.asInstanceOf[Ordering[k]], eR.asInstanceOf[Elem[r]])
//
//                //todo: type of a and b is Int, but Double expected. Check why it happens
//                val sL = res.asInstanceOf[PA[A]]
//                (t1 +^ (s -> sL), sL)
//              }
//            }
//            case PairArrayCompanionMethods.innerJoinNested(xss: NA[(k,a)] @unchecked, yss: NA[(_,b)] @unchecked, f@ Def(lam: Lambda[_,r]), ordK, eR) => ensureLiftedAll(t,s, xss ,yss) {
//              case (t1, Seq(xssL1, yssL1)) => {
//                val yssL = yssL1.asNA[PArray[(k,b)]]
//                val xssL = xssL1.asNA[PArray[(k,a)]]
//
//                val resFlat = PairArray.innerJoinNested(xssL.values, yssL.values, f.asRep[((a, b)) => r])(ordK.asInstanceOf[Ordering[k]], eR.asInstanceOf[Elem[r]])
//                val res = resFlat.nestBy(xssL.segments)
//
//                val sL = res.asInstanceOf[PA[A]]
//                (t1 +^ (s -> sL), sL)
//              }
//            }
//            case PairArrayCompanionMethods.outerJoinFetter(xs: PA[(k,a)] @unchecked, ys: PA[(_,b)] @unchecked, f@ Def(lam: Lambda[_,r]), ordK, eR) => ensureLiftedAll(t,s, xs ,ys) {
//              case (t1, Seq(xsL1, ysL1)) => {
//                val ysL = ysL1.asNA[(k,b)]
//                val xsL = xsL1.asNA[(k,a)]
//
//                val res = PairArray.outerJoinFetterNested(xsL, ysL, f.asRep[((a, b)) => r])(ordK.asInstanceOf[Ordering[k]], eR.asInstanceOf[Elem[r]])
//                val sL = res.asInstanceOf[PA[A]]
//                (t1 +^ (s -> sL), sL)
//              }
//            }
//            case d @ MethodCall(obj: Exp[a], method, args, neverInvoke) =>
//              ensureLiftedAllMethodCall(t, s, obj :: args) {
//                case (t1, ssL) =>
//                  val objL = ssL.head.asInstanceOf[F[a]]
//                  val argsL = ssL.tail
//                  implicit val eA: Elem[A] = d.elem.asInstanceOf[Elem[A]]
//                  val sL: PA[A] = PArray.liftMethodCall[a, A](objL, method, argsL, neverInvoke)
//                  (t1 +^ (s -> sL), sL)
//                case _ => !!!
//              }
//            case d @ PArrayCompanionMethods.liftMethodCall(objs, method, args, neverInvoke) =>
//              // Nearly certainly doesn't work correctly
//              d.elem match {
//                case eA: Elem[r] =>
//                  implicit val eA1: Elem[r] = eA
//                  ensureLiftedAllMethodCall(t, s, objs :: args) {
//                    case (t1, ssL) =>
//                      val zipped = zipLiftedMany(ssL.map(_.asInstanceOf[NA[_]]))
//                      val sL0: NA[r] = mapLifted(zipped) { zippedValues =>
//                        unzipMany(zippedValues, ssL.map(_.asInstanceOf[NA[_]])) match {
//                          case (objsL: PA[a] @unchecked) :: argsL =>
//                            PArray.liftMethodCall[a, r](objsL, method, argsL, neverInvoke)
//                          case _ => ???("Unexpected result from unzipMany")
//                        }
//                      }
//                      val sL = sL0.asPA[A]
//                      (t1 +^ (s -> sL), sL)
//                    case _ => !!!
//                  }
//              }
            case d => !!!(s"Don't know how to lift node: $s -> $d")
          }
          case _ => !!!(s"No Def found for $s")
        }
      case false =>
        super.apply[A](t, rw, s, d)
    }

    private def replicateSym[A](t: VectContext[K, I, C, F], s: Exp[A]): (VectContext[K, I, C, F], RF[A]) = {
      implicit val eA = s.elem
      val sL = t.replicate(s)
      (t +^ (s -> sL), sL)
    }

    private def freeVarLifter[A](ctx: VectContext[K, I, C, F], v: Exp[A]): (VectContext[K, I, C, F], RF[A]) = v match {
      case Def(lam: Lambda[a, b]) => {
        val v1 = v.asRep[a => b]
        val fL = ctx.replicateFunc(v1)(lam.eA, lam.eB)
        (ctx +^ (v1 -> fL), fL.asInstanceOf[RF[A]])
      }
      case _ => replicateSym(ctx, v)
    }

    private def ensureLifted[A, B](t: VectContext[K, I, C, F], s: Exp[B], a: Exp[A])(cont: (VectContext[K, I, C, F], RF[A]) => (VectContext[K, I, C, F], RF[B])): (VectContext[K, I, C, F], RF[_]) = {
      val some: Option[(VectContext[K, I, C, F], RF[A])] = t.getLifted(a) match {
        case Some(aL) => Some((t, aL.asRep[F[A]]))
        case None => None
      }
      (some match {
        case Some((t1, as)) => cont(t1, as)
        case None => freeVarLifter[B](t, s)
      }).asInstanceOf[(VectContext[K, I, C, F], RF[_])]
    }

    private def ensureLifted[A, B, T](t: VectContext[K, I, C, F], s: Exp[T], a: Exp[A], b: Exp[B])(cont: (VectContext[K, I, C, F], RF[A], RF[B]) => (VectContext[K, I, C, F], RF[T])): (VectContext[K, I, C, F], RF[T]) = {
      val some: Option[(VectContext[K, I, C, F], RF[A], RF[B])] = (t.getLifted(a), t.getLifted(b)) match {
        case (Some(aL), Some(bL)) => Some((t, aL.asRep[F[A]], bL.asRep[F[B]]))
        case (None, Some(bL)) =>
          val (t1, aL) = freeVarLifter(t, a)
          Some((t1, aL, bL.asRep[F[B]]))
        case (Some(aL), None) =>
          val (t1, bL) = freeVarLifter(t, b)
          Some((t1, aL.asRep[F[A]], bL))
        case (None, None) => None
      }
      some match {
        case Some((t1, as, bs)) => cont(t1, as, bs)
        case None => freeVarLifter(t, s)
      }
    }

    private def ensureLiftedAll[A](t: VectContext[K, I, C, F], s: Exp[A], as: Exp[_]*)(cont: (VectContext[K, I, C, F], List[Rep[F[_]]]) => (VectContext[K, I, C, F], Rep[F[A]])): (VectContext[K, I, C, F], Rep[F[A]]) = {
      val (t1, asL) = as.foldLeft((t, List.empty[Rep[F[_]]])) {
        case ((t, asL), a) =>
          t.getLifted(a) match {
            case Some(aL) => (t, asL :+ aL)
            case None =>
              val (t1, aL) = freeVarLifter(t, a)
              (t1, asL :+ aL)
          }
      }
      cont(t1, asL)
    }

    private def ensureLiftedAllMethodCall[A](t: VectContext[K, I, C, F], s: Exp[A], as: Seq[AnyRef])(cont: (VectContext[K, I, C, F], List[AnyRef]) => (VectContext[K, I, C, F], RF[A])): (VectContext[K, I, C, F], RF[A]) = {
      val (t1, asL) = as.foldLeft((t, List.empty[AnyRef])) {
        case ((t, asL), a) =>
          a match {
            case a: Exp[_] =>
              t.getLifted(a) match {
                case Some(aL) => (t, asL :+ aL)
                case None =>
                  val (t1, aL) = freeVarLifter(t, a)
                  (t1, asL :+ aL)
              }
            case _ =>
              (t, asL :+ a)
          }
      }
      cont(t1, asL)
    }

    private def ensureLiftedClosure[A](t: VectContext[K, I, C, F], s: Exp[A], f: Exp[_])(cont: (VectContext[K, I, C, F], List[Exp[_]], List[Rep[F[_]]]) => (VectContext[K, I, C, F], Rep[F[A]])): (VectContext[K, I, C, F], Rep[F[A]]) = {
      val Def(fLam: Lambda[_, _]) = f
      val fvs = fLam.freeVars.toList
      ensureLiftedAll(t, s, fvs: _*) {
        case (t1, fvsL) => cont(t1, fvs, fvsL)
      }
    }
  }

  def liftLambdaSym[K, I[_], C[_], F[_], A, B](
      t: VectContext[K, I, C, F], rw: Rewriter, lam: Exp[A => B])(implicit eK:Elem[K], eF: Vectorizable[K,I,C,F]): (VectContext[K, I, C, F], Rep[F[A] => F[B]]) = {
    val m = new ShapeLambdaMirror[K,I,C,F,A,B](lam)
    val (t1, lamL) = m.mirrorNode(t, rw, lam.getLambda, lam)
    // cast should be safe
    (t1, lamL.asRep[F[A] => F[B]])
  }

  def liftLambdaSym[K, I[_], C[_], F[_], A, B](lam: Exp[A => B])(implicit eK:Elem[K], eF: Vectorizable[K,I,C,F]): Rep[F[A] => F[B]] = {
    val (_, lamL) = liftLambdaSym(VectContext.empty[K,I,C,F], NoRewriting, lam)
    lamL
  }

  def liftLambdaSym[K, I[_], C[_], F[_], A, B](lam: Exp[A => B], rw: Rewriter)(implicit eK:Elem[K], eF: Vectorizable[K,I,C,F]): Rep[F[A] => F[B]] = {
    val (_, lamL) = liftLambdaSym(VectContext.empty[K,I,C,F], rw, lam)
    lamL
  }

//  class VectorizeMapMirror(forceFlatten: Boolean = false) extends Mirror[VectContext[F]] {
//    private case class MapNode[A, B](sym: Exp[A], arr: RF[B], func: Exp[B => A]) {
//      def mirror(t: VectContext[F]) = MapNode(sym, t(arr), t(func))
//    }
//
//    private def applyByMirroringInContext[A, B](t: VectContext[F], f: Exp[A => B], s: Exp[A]): Exp[B] = {
//      val Def(Lambda(_, _, x, y)) = f
//      val t1 = t + (x -> s)
//      val (t2, _) = DefaultMirror.mirrorNode(t1, NoRewriting, f)
//      t2(y)
//    }
//
//    private def vectorizeMapNode[A, B](t: VectContext[F], rw: Rewriter, mn: MapNode[A, B]): RF[A] = {
//      val mn1 = mn.mirror(t)
//      val f = mn1.func
//      val (_, fL) = liftLambdaSym(VectContext[F].Empty, rw, f)
//      val newApply = applyByMirroringInContext(t, fL, mn1.arr)
//      newApply
//    }
//
//    //    val flattenedMaps: Map[Exp[_], Exp[_]] = {
//    //      val mapNodes = graph.scheduleAll map { _.sym } collect { case MapThatNeedFlattening(m) => m }
//    //      val subst = mapNodes map { mn =>
//    //        val (_, newApply) = vectorizeMapNode(VectContext[F].Empty, NoRewriting, mn)
//    //        (mn.sym, newApply)
//    //      }
//    //      subst toMap
//    //    }
//
//    private object MapThatNeedFlattening {
//      def unapply[A](s: Exp[A]): Option[MapNode[A, _]] = s match {
//        case Def(PArrayMethods.mapBy(Def(PArrayMethods.flatten(source: RF[b])), f)) => forceFlatten match {
//          case true => Some(MapNode(s, source, f.asRep[b => A]))
//          case _ => None
//        }
//
//        case Def(PArrayMethods.mapBy(arr: RF[b], f)) =>
//          Some(MapNode(s, arr, f.asRep[b => A]))
//        //        case x@Def(PArrayMethods.mapBy(arr, f@Def(l: Lambda[_,_]))) if !isScalarOp(l) =>
//        //          Some(MapNode(x, arr.asRF[Any], f))
//
//        case _ => None
//      }
//    }
//
//    override def apply[A](t: VectContext[F], rw: Rewriter, s: Exp[A]): (VectContext[F], Exp[_]) =
//      s match {
//        case MapThatNeedFlattening(mapNode: MapNode[a, b]) => {
//          //val sV = vectorizeMapNode(t, rw, mapNode)
//          val mn1 = mapNode.mirror(t)
//          val (_, fL) = liftLambdaSym(t, rw, mn1.func)
//          val newApply = applyByMirroringInContext(t, fL, mn1.arr)
//          (t + (s -> newApply), newApply)
//        }
//        case Def(PArrayMethods.filterBy(xs, f @ Def(l: Lambda[a, _]))) /*if !isScalarOp(l)*/ => {
//          val xsL = t(xs.asRep[PArray[a]])
//          val (_, fL) = liftLambdaSym(VectContext[F].Empty, rw, t(f.asRep[a => Boolean]))
//          val flags = applyByMirroringInContext(t, fL, xsL)
//          val sV = xsL.filterByMask(flags)
//          (t + (s -> sV), sV)
//        }
//        case _ =>
//          super.apply(t, rw, s)
//      }
//  }
//
//  object VectorizeMapMirror extends VectorizeMapMirror(false)

  private def mirrorSubst(s1: Rep[_], rw: Rewriter, t: MapTransformer) = {
    val g1 = new PGraph(s1).transform(DefaultMirror, rw, t)
    (g1.mapping, g1.roots.head)
  }
}

trait ShapingCompiler[ScalanCake <: ScalanDslExp with ShapingDslExp] extends Compiler[ScalanCake] {
  import scalan._

  override def graphPasses(compilerConfig: CompilerConfig) =
    super.graphPasses(compilerConfig) ++
      Seq(AllInvokeEnabler,
        constantPass[ShapingPass]("shaping", b => ShapingPass(b, DefaultMirror, NoRewriting)))

  case class ShapingPass(val builder: PassBuilder[ShapingPass], mirror: Mirror[MapTransformer], rewriter: Rewriter) extends GraphPass {
    def name = "shaping"
//    override val config = PassConfig(shouldUnpackTuples = true)
    def apply(graph: PGraph): PGraph = {
      graph
//      graph.transform(mirror, rewriter, MapTransformer.Empty)
    }
  }

}

