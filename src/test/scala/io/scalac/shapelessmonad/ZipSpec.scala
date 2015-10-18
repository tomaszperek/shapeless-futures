package io.scalac.shapelessmonad

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Future._

/**
  * Created by tomasz on 18/10/15.
  */
class ZipSpec extends FlatSpec with ScalaFutures with Matchers {
  import scala.concurrent.ExecutionContext.Implicits.global
  import scalaz.Scalaz._

  "zip" should "provide one future from args of futures" in {
    val result = zip(successful(1), successful(true), successful("string"), successful(1.0))
    val (a, b, c, d) = result.futureValue
    (a, b, c, d) should equal((1, true, "string", 1.0))
  }

  it should "work with Lists too" in {
    zip(List(1, 2), List(3, 4), List(5, 6)) should equal(List((1,3,5), (1,3,6), (1,4,5), (1,4,6), (2,3,5), (2,3,6), (2,4,5), (2,4,6)))
  }
  it should "work with Options too" in {
    zip(Option(1), Option(3), Option(5)) should equal(Some(1,3,5))
  }

}
