package aggregate

import scala.language.implicitConversions

object AggregateSyntax extends AggregateAPI:

  private enum AggregateGrammar[A]:
    case Exchange[A, S](
        init: Aggregate[S],
        body: Aggregate[S] => (Aggregate[A], Aggregate[S])
    ) extends AggregateGrammar[A]
    case NFold[A, B](init: Aggregate[A], a: Aggregate[B], f: (A, B) => A)
        extends AggregateGrammar[A]
    case Call(f: Aggregate[() => Aggregate[A]])
    case Sensor(name: Aggregate[String])
    case Uid extends AggregateGrammar[Device]
    case Self(of: Aggregate[A])
    // More generally this will become UpdateDevice and then we can implement this through Map
    case UpdateSelf[A, B](fa: Aggregate[A], f: A => B)
        extends AggregateGrammar[B]
    case Map[A, B](fa: Aggregate[A], f: A => B) extends AggregateGrammar[B]
    case FlatMap[A, B](fa: Aggregate[A], f: A => Aggregate[B])
        extends AggregateGrammar[B]
  import AggregateGrammar.*

  import cats.free.Free
  opaque type Aggregate[A] = Free[AggregateGrammar, A]

  def sensor[A](name: Aggregate[String]): Aggregate[A] =
    Free.liftF(Sensor(name))

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] =
    Free.liftF(Call(f))

  def exchange[A, S](init: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] =
    Free.liftF(Exchange(init, f))

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] =
    Free.liftF(NFold(init, a, f))

  def uid: Aggregate[Device] =
    Free.liftF(Uid)

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A] = Free.liftF(Self(fa))
    def updateSelf(f: A => A): Aggregate[A] = Free.liftF(UpdateSelf(fa, f))
    def map[B](f: A => B): Aggregate[B] = Free.liftF(Map(fa, f))
    def flatMap[B](f: A => Aggregate[B]): Aggregate[B] =
      Free.liftF(FlatMap(fa, f))

  given pureGiven[A]: Conversion[A, Aggregate[A]] with
    def apply(x: A): Aggregate[A] = Free.pure(x)

//
// object Utils:
//   import AggregateSyntax.*
//   extension [A](a: Aggregate[A])
//     def self: Agg[A] =
//       for
//         mid <- mid
//         a <- a
//       yield a(mid)
//
//   def mux[A](
//       cond: Aggregate[Boolean],
//       the: Aggregate[A],
//       els: Aggregate[A]
//   ): Aggregate[A] =
//     for
//       cond <- cond.self
//       the <- the
//       els <- els
//     yield if cond then the else els
//
//   def branch[A](
//       cond: Aggregate[Boolean],
//       the: => Aggregate[A],
//       els: => Aggregate[A]
//   ): Aggregate[A] =
//     for
//       cond <- cond.self
//       res <- if cond then the else els
//     yield res
//
//   def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A] =
//     exchange(default, (n) => (n, send))
//
//   def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A]) = (a, a)
//
//   // TODO: unfortunately we cannot provice a given instance for math.Numeric
//   // since some functions return pure types.
//   // Example: compare[A](x: Aggregate[A], y: Aggregate[A]): Int
//   // This would force us to "exit" from the monad
//
//   private def evaluateAndApply[A, B](a: Aggregate[A], b: Aggregate[A])(
//       f: (NValue[A], NValue[A]) => NValue[B]
//   ): Aggregate[B] =
//     for
//       a <- a
//       b <- b
//     yield f(a, b)
//
//   private def nvNum[A: Numeric] = summon[Numeric[NValue[A]]]
//   extension [A: Numeric](a: Aggregate[A])
//     infix def +(b: Aggregate[A]): Aggregate[A] =
//       evaluateAndApply(a, b)(nvNum.plus)
//     infix def -(b: Aggregate[A]): Aggregate[A] =
//       evaluateAndApply(a, b)(nvNum.minus)
//     infix def *(b: Aggregate[A]): Aggregate[A] =
//       evaluateAndApply(a, b)(nvNum.times)
//
//   private def nvFrac[A: Fractional] = summon[Fractional[NValue[A]]]
//   extension [A: Fractional](a: Aggregate[A])
//     infix def /(b: Aggregate[A]): Aggregate[A] =
//       evaluateAndApply(a, b)(nvFrac.div)
