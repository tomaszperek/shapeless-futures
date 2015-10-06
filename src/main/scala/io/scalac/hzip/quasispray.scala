package io.scalac.hzip

import shapeless.{HNil, ::, HList}
import shapeless.ops.function.FnToProduct
import shapeless.ops.hlist.Prepend

/**
 * Created by tomasz on 08/10/15.
 */
object quasispray extends App{

  case class Response(body: String)
  case class Request(url : String)
  type Route = Request => Response
  def complete(f: => Response): Route = {
    ctx => f
  }

  trait Directive[X <: HList] { self =>
    def happly(f: X => Route): Route
    def apply[F](f: F)(implicit fp: FnToProduct.Aux[F, X => Route]) = {
      happly(fp(f))
    }
    def &[Y <: HList](that: Directive[Y])(implicit prepend: Prepend[X, Y]): Directive[prepend.Out] = new Directive[prepend.Out] {
      override def happly(f: prepend.Out => Route): Route = {
        self.happly( x =>
          that.happly( y =>
            f(prepend(x, y))
          )
        )
      }
    }
  }

  def host: Directive[String :: HNil] = {
    extract { case Request(url) =>
      val Array(protocol, rest) = url.split("://")
      rest.split("/", 1).head :: HNil
    }
  }
  def protocol: Directive[String :: HNil] = {
    extract { case Request(url) =>
      val Array(protocol, rest) = url.split("://")
      protocol :: HNil
    }
  }

  def extract[L <: HList](f: Request => L): Directive[L] = new Directive[L] {
    def happly(inner: L ⇒ Route): Route = ctx ⇒ inner(f(ctx))(ctx)
  }

  val protocolAndHost: Directive[String :: String :: HNil] = protocol & host

  val route = protocolAndHost { (p: String, h: String) =>
    complete {
      Response(s"protocol was $p and host was $h")
    }
  }


  println(route(Request("http://www.scalac.io")))


}
