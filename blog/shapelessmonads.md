# Shapeless Monads

## <a href="#small-retro" name="small-retro">Small retrospection</a>

In my <a href="http://blog.scalac.io/2015/10/15/shapeless-and-futures.html">previous blog post</a> we talked about shapless and how it could be applied to enhance your working with `Futures`. Even thouth we were focused on `Future`s, my goal wasn't to provide the best and ulitmate util to deal with them, but to show you how shapeless can help you build functions that are more flexible than almost everything you are used to work with. So in last post we have created kind-of varargs function that is able to adjust it's return type to the arguments passed in. Today we'll take it much farther by adding scalaz, `ApplicativeBuilder` and `Monad`s into the soup.

## <a href="#scalaz" name="scalaz">Scalaz? Applicative Builder?</a>

Yes, Scalaz, Applicative Builder... Scalaz is library that, to quote the definition the authors have given, "provides purely functional data structures to complement those from the Scala standard library. It defines a set of foundational type classes (e.g. Functor, Monad) and corresponding instances for a large number of data structures". Most people associate it with its set of cryptic operators to work with these "purely functional data structures": `|@|` (so called <a href="http://vignette3.wikia.nocookie.net/p__/images/5/5d/Home_alone_macaulay_culkin_kevin_mccallister_boy_fear_shout_fright_346_1600x1200.jpg/revision/latest?cb=20140320134854&path-prefix=protagonist">Macaulay Culkin</a>), `<*>`, `<|` etc.  and it's what scares the hell out of many people. I find it unfair. In fact, scalaz privides many very useful utils (some of which are actually very basic), type classes and other concepts and I highly encourage you to get familiar with it. <a href="http://eed3si9n.com/learning-scalaz/">This</a> is probably one of the best training resources on the web. We won't even try to walk through every aspect of it (is there even a man who would be able to do it?). We are mentioning it, because some people pointed out that the same or similar effect could be obtaining using scalaz's `ApplicativeBuilder` pattern. This is an excerpt from comment by @caente aiming to support this opinion:
```scala
scala> import scalaz._, Scalaz._
import scalaz._
import Scalaz._

scala> import scala.concurrent.Future
import scala.concurrent.Future

scala> import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext.Implicits.global

scala> (Future(1) |@| Future(2) |@| Future(3)){
     | (x,y,z) => x + y + z
     | }
res1: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@12f00066

scala> (Future(1) |@| Future(2) ){
     | (x,y) => x + y
     | }
res2: scala.concurrent.Future[Int] = scala.concurrent.impl.Promise$DefaultPromise@6aea1830

scala> (Future(1) |@| Future("2") ){
     | (x,y) => x.toString + y
     | }
res3: scala.concurrent.Future[String] = scala.concurrent.impl.Promise$DefaultPromise@7b515368
```
 Well, my answer for this claim is "yes, but no" :) It's not exactly the same thing, and it's more verbose from one point of view, yet more flexible from the other, as it allows you to apply a function right away. Important thing is that this comment actually gave me an idea for this post. We'll focus on `ApplicativeBuilder` to prove some thesis.

## <a href="#thesis" name="thesis">Thesis</a>

So here's the thesis: *With shapeless you can create code, that is much more flexible, and much more compact than it would be if you didn't use shapeless*. It won't intoduce new design patterns or paradigms nor will make any old ones obsolete, yet there will be big benefit in much less lines of code and much bigger flexibility. In order to advocate for this thesis we will try to implement our own version of `ApplicativeBuilder`

## <a href="#first-steps" name="first-steps">First steps</a>

Before we start, let's say what's wrong with the code from last post. It's only for `Futures`. The advantage `ApplicativeBuilder` has over `hsequence` / `zip` functions is that it works with any Applicative. So it will work with `Option`s, `List`s, not only `Future`s. `zip` is only for `Future`s. So let's remove this constraint.

Our base trait will now look as follows:
```scala
trait IsHListOfM[M[_], In <: HList, Out <: HList] extends ToMonadOps{
	def hsequence(l: In): M[Out]
}
```
what has changed? Besides name, which now reflects that this thing is not intended to work only with `Future`s, the trait has one more type parameter, `M[_]`, a type constructor, and the return value of `hsequence` method changed from `Future[Out]` to `M[Out]`. Let's not pay attention to `MonadOps` for a while, it's enabling some implicit conversions of instances of `M` and actually makes no impact on the subject here.
Now, traditionally, implementation for `HNil`:
```scala
implicit def IsHNilHListOfM[M[_]](implicit m: Monad[M]) = new IsHListOfM[M, HNil, HNil] {
	override def hsequence(l: HNil): M[HNil] = m.pure(HNil)
}
```

First of all, since `object`s can't have type parameters, we must use `def`, but that's not a big deal is it? The other thing new is evidence `m: Monad[M]`. This puts a constraint on `M`, we want it to belong to `Monad` type class. It's because we need access to `bind`, `pure`, and `map` functions. In the code above we use `m.pure(HNil)` to construct `M[HNil]`. We couldn't use constructor, because we simply don't know what exact type `M` will be.

What about longer `HList`s? Here's the code:

```scala
implicit def hconsIsHListOfM[M[_], H, In <: HList, Out <: HList](implicit ev: IsHListOfM[M, In, Out], m: Monad[M]): IsHListOfM[M, M[H] :: In, H :: Out] =
new IsHListOfM[M, M[H] :: In, H :: Out] {
      override def hsequence(l: M[H] :: In): M[H :: Out] =
        l.head.flatMap(h => ev.hsequence(l.tail).map(h :: _))
    }
```

It's getting a bit crowded in type parameters section, and in implicit params too, but there's only one new thing: `M[_]` parameter and evidence that it is a `Monad`. The implementationd of `hsequence` isn't really that much different from what we had for `Future`s. We use `flatMap` (that's why we needed `M` to be a `Monad`, not just `Aplicative`) and `map` just as we had with `Future`s. Let's put it together:

```scala
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
```
All in all it's just a little bit different than it was before. The same goes for implementations of `hsequence` and `zip` funtions. We are introducing one more type parameter, `M[_]` and providing one more evidence, that `M` is a `Monad`:
```scala
  def hsequence[M[_], In <: HList, Out <: HList](l: In)(implicit ev: IsHListOfM[M, In, Out], m: Monad[M]) = ev.hsequence(l)

  def zip[M[_], P <: Product, In <: HList, Out <: HList]
  (p: P)
  (implicit gen: Generic.Aux[P, In], ev: IsHListOfM[M, In, Out], tupler: Tupler[Out], m: Monad[M]) = {
    m.map(hsequence(gen.to(p)))(_.tupled)
  }
```
But now we won't be limited to `Futures`:
```scala
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
```
It may be a bit surprising how does it work with `List`s, but's exactly the same as if you used `|@|` from scalaz.
One challenge solved, we have our `zip`/`hsequence` working with everything that belongs to `Monad` type class.

## <a href="#applicativebuilder" name="applicativebuilder">Our Own Applicative Builder</a>

We are now just one step away from giving life to our own `ApplicativeBuilder`. But before we do it, let's recall what it actually is. Well, it's a trait, that allows you to combine `Applicative`s using `|@|` operator and then either return tuple with combined applicative or execute some function on it, just as it was demonstrated in @caente's comment. Now, why are we reimplementing it?. This is why:
<a href="https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/syntax/ApplicativeBuilder.scala"> its implementation</a>. The code is ~170 lines long and it's a pyramid that is just begginh to be simplified. You have just one mean to do it, and it's type-level programming. I won't make you wait any longer - here's the code:
```scala
case class ScalacApplicativeBuilder[M[_], In <: HList, Out <: HList](values: In)(implicit m: Monad[M]) {
  def asTuple[T](implicit  ev: IsHListOfM[M, In, Out], m: Monad[M], tupler: Tupler.Aux[Out, T]): M[T] = m.map(hsequence(values))(_.tupled)

  def apply[F, FOut](f: F)(implicit fnEv: FnToProduct.Aux[F, Out => FOut],  ev: IsHListOfM[M, In, Out]): M[FOut] =
    m.map(hsequence(values))(fnEv(f))

  def :@:[X, T1](newOne: M[X]) = ScalacApplicativeBuilder[M, M[X] :: In, X :: Out](newOne :: values)
}
```
We only need an implicit def to convert `Monad`s to single-element `ScalacApplicativeBuilder`:
```scala
implicit def ToScalacApplicativeBuilder[M[_], V](value: M[V])(implicit ev: IsHListOfM[M, M[V] :: HNil, V :: HNil], m: Monad[M]): ScalacApplicativeBuilder[M, M[V] :: HNil, V::HNil] =
    new ScalacApplicativeBuilder[M, M[V] :: HNil, V :: HNil](value :: HNil)
```
and that's actually it.

How does it work? First important thing is that it maintains a `HList` of `Monad`s in `values` field. `:@:` just adds one more item this list. `asTuple` is just calling `hsequece` on values and turns the result into `Tuple`. `apply` transforms given function argument into `HList` equivalent and then maps results of `hsequence` with that equivalent. And that's that.

Now, did we prove our thesis? I think we did. Together with imports, the full and standalone implementation of our `ScalacApplicativeBuilder` is only 42 lines long:
```scala
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
```
It allows doing the same things as original Applicative Builder and is not limited to 12 elements. Let's just take a quick look on specs:
```scala
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
```

As we can see, we can do what we could with original `ApplicativeBuilder` and more. The only drawback is that original one worked with members of 'Apply' type class, and we for sake of making our lives easier, implemented it for 'Monad'. It would be a nice exercise to rewrite it, but taking into account what we needed to show here, it's ok like that.

So that's a **QED** :) I hope you enjoyed reading this post as much as I did writing it, but what I hope even more, is that I have managed to convince at least some people to look kindly on shapeless.

Thank you!




