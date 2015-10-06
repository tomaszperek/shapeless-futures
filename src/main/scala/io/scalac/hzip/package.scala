package io.scalac

import shapeless._
import shapeless.ops.product.ToHList

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

package object hzip {

  sealed trait HZippable[H <: HList] {
    type Out <: HList

    def hzip: Future[Out]
  }

  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  implicit class HNilZippable(x: HNil) extends HZippable[HNil] {
    type Out = HNil

    def hzip: Future[HNil] = Future.successful(HNil)
  }


  implicit class HConsHZippable[H, T <: HList <% HZippable[T]](l: Future[H] :: T)
                                  (implicit ec: ExecutionContext)
    extends HZippable[Future[H] :: T] {

    val tzippable: HZippable[T] = l.tail

    type Out = H :: tzippable.Out

    def hzip: Future[Out] = {
      val f = l.head
      val p = scala.concurrent.Promise[H :: tzippable.Out]
      f.onComplete {
        case Success(e) => tzippable.hzip onComplete { zipped =>
          p complete zipped.map(e :: _)
        }
        case Failure(ex) => p.failure(ex)
      }
      p.future
    }
  }


  def hzip[P <: Product, H, T <: HList <% HZippable[T]](p: P)(implicit toHList: ToHList.Aux[P, Future[H] :: T]) =
    toHList(p).hzip



}
