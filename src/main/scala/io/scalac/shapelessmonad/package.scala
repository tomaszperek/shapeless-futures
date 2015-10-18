package io.scalac

import shapeless._
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.Tupler
import scala.languageFeature.implicitConversions
import scalaz.Monad
import scalaz.syntax.ToMonadOps

package object shapelessmonad {

  trait IsHListOfM[M[_], In <: HList, Out <: HList] extends ToMonadOps{
    def hsequence(l: In): M[Out]
  }

  object IsHListOfM {
    def apply[M[_], In <: HList, Out <: HList](implicit isHM: IsHListOfM[M, In, Out], m: Monad[M]): IsHListOfM[M, In, Out] = isHM

    implicit def IsHNilHListOfM[M[_]](implicit m: Monad[M]) = new IsHListOfM[M, HNil, HNil] {
      override def hsequence(l: HNil): M[HNil] = m.pure(HNil)
    }

    implicit def hconsIsHListOfM[M[_], H, In <: HList, Out <: HList](implicit ev: IsHListOfM[M, In, Out], m: Monad[M]): IsHListOfM[M, M[H] :: In, H :: Out] = new IsHListOfM[M, M[H] :: In, H :: Out] {
      override def hsequence(l: M[H] :: In): M[H :: Out] =
        l.head.flatMap(h => ev.hsequence(l.tail).map(h :: _))
    }
  }

  def zip[M[_], P <: Product, In <: HList, Out <: HList]
  (p: P)
  (implicit gen: Generic.Aux[P, In], ev: IsHListOfM[M, In, Out], tupler: Tupler[Out], m: Monad[M]) = {
    m.map(hsequence(gen.to(p)))(_.tupled)
  }

  def -->[M[_], P <: Product, In <: HList, Out <: HList, F, Out1]
  (p: P)(f: F)
  (implicit gen: Generic.Aux[P, In],
   ev: IsHListOfM[M, In, Out],
   tupler: Tupler[Out],
   m: Monad[M],
   fnEv: FnToProduct.Aux[F, Out => Out1]
  ): M[Out1] = {
    m.map(hsequence(gen.to(p)))(fnEv(f))
  }

  def hsequence[M[_], In <: HList, Out <: HList](l: In)(implicit ev: IsHListOfM[M, In, Out], m: Monad[M]) = ev.hsequence(l)

  case class ScalacApplicativeBuilder[M[_], In <: HList, Out <: HList](values: In)(implicit m: Monad[M]) {
    def asTuple[T](implicit  ev: IsHListOfM[M, In, Out], m: Monad[M], tupler: Tupler.Aux[Out, T]): M[T] = m.map(hsequence(values))(_.tupled)

    def apply[F, FOut](f: F)(implicit fnEv: FnToProduct.Aux[F, Out => FOut],  ev: IsHListOfM[M, In, Out]): M[FOut] =
      m.map(hsequence(values))(fnEv(f))

    def :@:[X, T1](newOne: M[X]) = ScalacApplicativeBuilder[M, M[X] :: In, X :: Out](newOne :: values)
  }

  implicit def ToScalacApplicativeBuilder[M[_], V](value: M[V])(implicit ev: IsHListOfM[M, M[V] :: HNil, V :: HNil], m: Monad[M]): ScalacApplicativeBuilder[M, M[V] :: HNil, V::HNil] =
    new ScalacApplicativeBuilder[M, M[V] :: HNil, V :: HNil](value :: HNil)
}
