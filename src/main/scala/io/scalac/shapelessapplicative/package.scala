package io.scalac

import shapeless._
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.Tupler
import scala.languageFeature.implicitConversions
import scalaz.Applicative

package object shapelessapplicative {

  trait IsHListOfM[M[_], In <: HList, Out <: HList] {
    def hsequence(l: In): M[Out]
  }

  object IsHListOfM {
    def apply[M[_], In <: HList, Out <: HList](implicit isHM: IsHListOfM[M, In, Out], A: Applicative[M]): IsHListOfM[M, In, Out] = isHM

    implicit def IsHNilHListOfM[M[_]](implicit A: Applicative[M]) = new IsHListOfM[M, HNil, HNil] {
      override def hsequence(l: HNil): M[HNil] = A.pure(HNil)
    }

    implicit def hconsIsHListOfM[M[_], H, In <: HList, Out <: HList](implicit ev: IsHListOfM[M, In, Out], A: Applicative[M]): IsHListOfM[M, M[H] :: In, H :: Out] = new IsHListOfM[M, M[H] :: In, H :: Out] {
      override def hsequence(l: M[H] :: In): M[H :: Out] = {
        val h = l.head
        val tl = ev.hsequence(l.tail)
        A.apply2(h, tl)(_ :: _)
      }
    }
  }

  def zip[M[_], P <: Product, In <: HList, Out <: HList]
  (p: P)
  (implicit gen: Generic.Aux[P, In], ev: IsHListOfM[M, In, Out], tupler: Tupler[Out], A: Applicative[M]) = {
    A.map(hsequence(gen.to(p)))(_.tupled)
  }

  def -->[M[_], P <: Product, In <: HList, Out <: HList, F, Out1]
  (p: P)(f: F)
  (implicit gen: Generic.Aux[P, In],
   ev: IsHListOfM[M, In, Out],
   tupler: Tupler[Out],
   A: Applicative[M],
   fnEv: FnToProduct.Aux[F, Out => Out1]
  ): M[Out1] = {
    A.map(hsequence(gen.to(p)))(fnEv(f))
  }

  def hsequence[M[_], In <: HList, Out <: HList](l: In)(implicit ev: IsHListOfM[M, In, Out], A: Applicative[M]) = ev.hsequence(l)

  case class ScalacApplicativeBuilder[M[_], In <: HList, Out <: HList](values: In)(implicit A: Applicative[M]) {
    def asTuple[T](implicit  ev: IsHListOfM[M, In, Out], A: Applicative[M], tupler: Tupler.Aux[Out, T]): M[T] = A.map(hsequence(values))(_.tupled)

    def apply[F, FOut](f: F)(implicit fnEv: FnToProduct.Aux[F, Out => FOut],  ev: IsHListOfM[M, In, Out]): M[FOut] =
      A.map(hsequence(values))(fnEv(f))

    def :@:[X, T1](newOne: M[X]) = ScalacApplicativeBuilder[M, M[X] :: In, X :: Out](newOne :: values)
  }

  implicit def ToScalacApplicativeBuilder[M[_], V](value: M[V])(implicit ev: IsHListOfM[M, M[V] :: HNil, V :: HNil], A: Applicative[M]): ScalacApplicativeBuilder[M, M[V] :: HNil, V::HNil] =
    new ScalacApplicativeBuilder[M, M[V] :: HNil, V :: HNil](value :: HNil)
}
