enum AggregateGrammar[A]:
  case Exchange[A, S](init: S, body: NValue[S] => (A, NValue[S]))
      extends AggregateGrammar[A]
  case Branch(cond: Boolean, the: Aggregate[A], els: Aggregate[A])
import AggregateGrammar.*

import cats.free.Free
type Aggregate[A] = Free[AggregateGrammar, A]

def exchange[R, S](init: S, body: NValue[S] => (R, NValue[S])): Aggregate[R] =
  Free.liftF(Exchange(init, body))

def branch[A](
    cond: Boolean,
    the: Aggregate[A],
    els: Aggregate[A]
): Aggregate[A] =
  Free.liftF(Branch(cond, the, els))

// Utils

def nfold[A, B](f: (A, B) => A, nValue: NValue[B], init: A): A = ???

def nbr[A](default: A, send: A): Aggregate[NValue[A]] =
  exchange(default, (n) => (n, send))

def retsend[A](a: A): (A, NValue[A]) = (a, a)

def mux[A](cond: Boolean, th: => A, el: => A): A =
  val t = th
  val e = el
  if cond then t else e

def plus[N: Numeric as num](n1: N, n2: N): N =
  num.plus(n1, n2)
