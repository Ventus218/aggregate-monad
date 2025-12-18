import NValues.*
import scala.language.implicitConversions

enum AggregateGrammar[A]:
  case Exchange[A, S](
      init: S,
      body: NValue[S] => (Aggregate[A], Aggregate[NValue[S]])
  ) extends AggregateGrammar[A]
  case Branch(cond: Boolean, the: () => Aggregate[A], els: () => Aggregate[A])
  case Self extends AggregateGrammar[Device]
import AggregateGrammar.*

import cats.free.Free
type Aggregate[A] = Free[AggregateGrammar, A]

def exchange[R, S](
    init: Aggregate[S],
    body: NValue[S] => (Aggregate[R], Aggregate[NValue[S]])
): Aggregate[R] =
  for
    init <- init
    res <- Free.liftF[AggregateGrammar, R](Exchange(init, body))
  yield res

// def branch[A](
//     cond: Boolean,
//     the: => Aggregate[A],
//     els: => Aggregate[A]
// ): Aggregate[A] =
//   Free.liftF(Branch(cond, () => the, () => els))

def branch[A](
    cond: Aggregate[Boolean],
    the: => Aggregate[A],
    els: => Aggregate[A]
): Aggregate[A] =
  for
    cond <- cond
    res <- branch(cond, the, els)
  yield res

def self: Aggregate[Device] =
  Free.liftF(Self)

// Utils

def nfold[A, B](
    f: (A, B) => A,
    nValue: Aggregate[NValue[B]],
    init: Aggregate[A]
): Aggregate[A] =
  for
    self <- self
    nValue <- nValue
    init <- init
  yield (nValue.toMap.removed(self).values.foldLeft(init)(f))

def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[NValue[A]] =
  // for
  //   default <- default
  //   send <- send
  //   res <- exchange(default, (n) => (n, send))
  // yield (res)
  exchange(default, (n) => (n, send.map(NValue(_))))

def retsend[A](a: A): (A, NValue[A]) = (a, a)

def mux[A](
    cond: Aggregate[Boolean],
    th: Aggregate[A],
    el: Aggregate[A]
): Aggregate[A] =
  for
    cond <- cond
    th <- th
    el <- el
  yield (if cond then th else el)

def plus[N: Numeric as num](n1: N, n2: N): N =
  num.plus(n1, n2)

given [A]: Conversion[A, Aggregate[A]] with
  def apply(x: A): Aggregate[A] = Free.pure(x)
