package io.scalac.hzip

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures

class ZipSpec extends FlatSpec with Matchers with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.Future._
  "zip" should "provide one future from args of futures" in {
    val result = zip(successful(1), successful(true), successful("string"), successful(1.0))
    val (a, b, c, d) = result.futureValue
    (a, b, c, d) should equal((1, true, "string", 1.0))
  }
  it should "work well with failures" in {
    val exception: Exception = new scala.Exception("booo")
    val result = zip(successful(1), successful(true), failed(exception), successful(1.0))
    whenReady(result.failed) { ex ⇒
      ex should equal(exception)
    }
  }

  it should "allow you to go crazy" in {
    val (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = zip(
      successful(1),
      successful(2),
      successful(false),
      successful(true),
      successful("someString"),
      successful(BigDecimal(0.0)),
      successful(1.1),
      successful('andSymbolsToo),
      successful("future havoc"),
      successful(10),
      successful(11),
      successful(12),
      successful(13),
      successful("enough already")
    ).futureValue

    a should equal(1)
    b should equal(2)
    c should equal(false)
    d should equal(true)
    e should equal("someString")
    f should equal(BigDecimal(0.0))
    g should equal(1.1)
    h should equal('andSymbolsToo)
    i should equal("future havoc")
    j should equal(10)
    k should equal(11)
    l should equal(12)
    m should equal(13)
    n should equal("enough already")

  }

  it should "not compile if hlist has non-future element" in {
    """
      |val result = zip(successful(1), successful(true), successful("Some string"))
    """.stripMargin should compile

    """
      |val result = zip(successful(1), successful(true), "Some string")
    """.stripMargin shouldNot compile
  }
}
