import NValues.*
import scala.language.implicitConversions
import cats.syntax.flatMap
import AggregateSyntax.Aggregate

object AggregateSyntax:
  private case class Env(
      mid: Device,
      alignedNbrs: Set[Device],
      sensors: Map[String, Any]
  )
  private enum AggregateGrammar[A]:
    case Exchange[A, S](
        init: Aggregate[S],
        body: Aggregate[S] => (Aggregate[A], Aggregate[S])
    ) extends AggregateGrammar[NValue[A]]
    case Call(f: () => Aggregate[A]) extends AggregateGrammar[NValue[A]]
    case Builtin(f: (Env) => Agg[A]) extends AggregateGrammar[A]
  import AggregateGrammar.*

  import cats.free.Free
  opaque type Agg[A] = Free[AggregateGrammar, A]
  type Aggregate[A] = Agg[NValue[A]]

  extension [A](fa: Agg[A])
    def flatMap[B](f: A => Agg[B]): Agg[B] =
      fa.flatMap(f)
    def map[B](f: A => B): Agg[B] =
      fa.map(f)

  object Aggregate:
    def apply[A](x: A): Aggregate[A] =
      Free.pure(NValue(x))
  object Agg:
    def apply[A](x: A): Agg[A] =
      Free.pure(x)

  given [A]: Conversion[A, Aggregate[A]] with
    def apply(x: A): Aggregate[A] = Aggregate(x)

  def exchange[R, S](
      init: Aggregate[S],
      body: Aggregate[S] => (Aggregate[R], Aggregate[S])
  ): Aggregate[R] =
    Free.liftF(Exchange(init, body))

  def call[A](f: () => Aggregate[A]): Aggregate[A] =
    Free.liftF(Call(f))

  def mid: Agg[Device] =
    Free.liftF(Builtin(env => Free.pure(env.mid)))

  def sensor[A](name: Aggregate[String]): Agg[A] =
    for
      str <- name.self
      res <- Free.liftF(Builtin(env => Free.pure(env.sensors(str))))
    yield res.asInstanceOf[A]

  // TODO: is using NValues for f too restrictive? should/can we use Aggregate
  def nfold[A, B](init: Aggregate[A])(nValue: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] =
    Free.liftF(
      Builtin(env =>
        for
          init <- init.self
          nValue <- nValue
          res = nValue.values.view
            .filterKeys(n => env.alignedNbrs.contains(n))
            .values
            .foldLeft(init)(f)
        yield NValue(res)
      )
    )

  export Utils.{*, given}

object Utils:
  import AggregateSyntax.*
  extension [A](a: Aggregate[A])
    def self: Agg[A] =
      for
        mid <- mid
        a <- a
      yield a(mid)

  def mux[A](
      cond: Aggregate[Boolean],
      the: Aggregate[A],
      els: Aggregate[A]
  ): Aggregate[A] =
    for
      cond <- cond.self
      the <- the
      els <- els
    yield if cond then the else els

  def branch[A](
      cond: Aggregate[Boolean],
      the: => Aggregate[A],
      els: => Aggregate[A]
  ): Aggregate[A] =
    for
      cond <- cond.self
      res <- if cond then the else els
    yield res

  def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A] =
    exchange(default, (n) => (n, send))

  def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A]) = (a, a)

  // TODO: unfortunately we cannot provice a given instance for math.Numeric
  // since some functions return pure types.
  // Example: compare[A](x: Aggregate[A], y: Aggregate[A]): Int
  // This would force us to "exit" from the monad

  private def evaluateAndApply[A, B](a: Aggregate[A], b: Aggregate[A])(
      f: (NValue[A], NValue[A]) => NValue[B]
  ): Aggregate[B] =
    for
      a <- a
      b <- b
    yield f(a, b)

  private def nvNum[A: Numeric] = summon[Numeric[NValue[A]]]
  extension [A: Numeric](a: Aggregate[A])
    infix def +(b: Aggregate[A]): Aggregate[A] =
      evaluateAndApply(a, b)(nvNum.plus)
    infix def -(b: Aggregate[A]): Aggregate[A] =
      evaluateAndApply(a, b)(nvNum.minus)
    infix def *(b: Aggregate[A]): Aggregate[A] =
      evaluateAndApply(a, b)(nvNum.times)

  private def nvFrac[A: Fractional] = summon[Fractional[NValue[A]]]
  extension [A: Fractional](a: Aggregate[A])
    infix def /(b: Aggregate[A]): Aggregate[A] =
      evaluateAndApply(a, b)(nvFrac.div)
