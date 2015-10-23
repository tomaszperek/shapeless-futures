package io.scalac.shapelessapplicative

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers}
import shapeless.HNil

import scala.concurrent.Future

/**
  * Created by tomasz on 17/10/15.
  */
class AppbuilderSpecs extends FlatSpec with Matchers with ScalaFutures {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scalaz.Scalaz._

  "Appbuilder" should "work" in {
    val optionsAB = Option(1) :@: Option(2)
    optionsAB((_: Int) + (_: Int)) should equal(Some(3))

    (Future(1) :@: Future(2) :@: Future("3")) { (a: Int, b: Int, c: String) =>
      a + b + c.toInt
    }.futureValue should equal(6)

    (List(1,2) :@: List(3,4) :@: List(5,6)) {
      (a: Int, b: Int, c: Int) => a + b + c
    } should equal(List(9, 10, 10, 11, 10, 11, 11, 12))

    (1.some :@: 2.some :@: 3.some :@: 4.some :@: 5.some :@: 6.some :@: 7.some :@: 8.some :@: 9.some :@: 10.some :@: 11.some :@: 12.some :@: 13.some :@: 14.some :@: 15.some :@: 16.some :@: 17.some :@: 18.some :@: 19.some :@: 20.some :@: 21.some :@: 22.some)
      .asTuple should equal((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22).some)
  }
}
