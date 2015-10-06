import shapeless._


import scala.concurrent.Future._
import scala.concurrent.duration._
import scala.concurrent.Await

val fs = successful(1) :: successful(2) :: successful(true) :: HNil

Await.result(fs.hzip, 1.second)
