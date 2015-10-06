package io.scalac.hzip

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
class HsequenceSpec extends FlatSpec with Matchers with ScalaFutures {
  import shapeless._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future._
  "hsequence" should "calculate hzip of hlist" in {
    val fs = successful(1) :: successful(true) :: successful("string") :: successful(1.0) :: HNil
    val a :: b :: c :: d :: HNil = hsequence(fs).futureValue
    a :: b :: c :: d :: HNil should equal(1 :: true :: "string" :: 1.0 :: HNil)
  }
  it should "work well with failures" in {
    val exception: Exception = new scala.Exception("booo")
    val fs = successful(1) :: successful(true) :: failed(exception) :: successful(1.0) :: HNil
    val result = hsequence(fs)
    whenReady(result.failed) { ex ⇒
      ex should equal(exception)
    }
  }


  it should "not compile if hlist has non-future element" in {
    """
      |val fs = successful(1) :: successful(true) :: successful("Some string") :: HNil
      |val result = hsequence(fs)
    """.stripMargin should compile

    """
      |val fs = successful(1) :: successful(true) :: "Some string" :: HNil
      |val result = hsequence(fs)
    """.stripMargin shouldNot compile
  }



}
