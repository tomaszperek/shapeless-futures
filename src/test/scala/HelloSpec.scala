import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import scalaz.Scalaz._

class HelloSpec extends FlatSpec with Matchers with ScalaFutures {
  import io.scalac.hzip._
  import shapeless._
  import scala.concurrent.Future._
  "hzip" should "calculate hzip of hlist" in {
    val fs = successful(1) :: successful(true) :: successful("string") :: successful(1.0) :: HNil
    val a :: b :: c :: d :: HNil = fs.hzip.futureValue
    a :: b :: c :: d :: HNil should equal(1 :: true :: "string" :: 1.0 :: HNil)
  }
  it should "work well with failures" in {
    val exception: Exception = new scala.Exception("booo")
    val fs = successful(1) :: successful(true) :: failed(exception) :: successful(1.0) :: HNil
    fs.hzip.eitherValue should equal(exception.left)
  }



}
