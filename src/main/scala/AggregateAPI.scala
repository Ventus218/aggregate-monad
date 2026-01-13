package aggregate

trait AggregateAPI:
  type Device
  type Aggregate[_]

  def sensor[A](name: Aggregate[String]): Aggregate[A]
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A]

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A]

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A]

  def uid: Aggregate[Device]

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A]
    def updateSelf(f: A => A): Aggregate[A]

  extension [A](fa: Aggregate[A]) def map[B](f: A => B): Aggregate[B]

  def pointwise[A, B](
      a: Aggregate[A],
      b: Aggregate[A],
      f: (A, A) => B
  ): Aggregate[B]

  given pureGiven[A]: Conversion[A, Aggregate[A]]

object AggregateAPI extends AggregateAPI:
  opaque type Device = Int
  export aggregate.simple.SimpleEffect.{
    Aggregate,
    sensor,
    call,
    exchange,
    nfold,
    uid,
    self,
    updateSelf,
    map,
    mux,
    pointwise
  }

  given pureGiven[A]: Conversion[A, Aggregate[A]] =
    aggregate.simple.SimpleEffect.pureGiven

  object Device:
    def fromInt(i: Int): Device = i

trait AggregateLib:
  import AggregateAPI.*
  def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A]

  def branch[A](cond: Aggregate[Boolean])(the: => Aggregate[A])(
      els: => Aggregate[A]
  ): Aggregate[A]

  def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A])

  extension [A, B](a: (Aggregate[A], Aggregate[B]))
    def fst: Aggregate[A]
    def snd: Aggregate[B]

  // Equals
  extension [A](a: Aggregate[A])
    infix def eq(b: Aggregate[A]): Aggregate[Boolean]

  // Logic operators
  extension (a: Aggregate[Boolean])
    infix def &(b: Aggregate[Boolean]): Aggregate[Boolean]
    infix def |(b: Aggregate[Boolean]): Aggregate[Boolean]

  // MATH
  extension [A: Numeric](a: Aggregate[A])
    infix def +(b: Aggregate[A]): Aggregate[A]
    infix def -(b: Aggregate[A]): Aggregate[A]
    infix def *(b: Aggregate[A]): Aggregate[A]
    infix def <(b: Aggregate[A]): Aggregate[Boolean]
    infix def >(b: Aggregate[A]): Aggregate[Boolean]
    infix def <=(b: Aggregate[A]): Aggregate[Boolean]
    infix def >=(b: Aggregate[A]): Aggregate[Boolean]

  extension [A: Fractional](a: Aggregate[A])
    infix def /(b: Aggregate[A]): Aggregate[A]

object AggregateLib extends AggregateLib:
  import AggregateAPI.*
  def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A] =
    exchange(default)(a => (a, send))

  def branch[A](cond: Aggregate[Boolean])(the: => Aggregate[A])(
      els: => Aggregate[A]
  ): Aggregate[A] =
    import AggregateAPI.pureGiven
    call:
      mux(cond)(pureGiven(() => the))(pureGiven(() => els))

  def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A]) =
    (a, a)

  extension [A, B](a: (Aggregate[A], Aggregate[B]))
    def fst: Aggregate[A] = a._1
    def snd: Aggregate[B] = a._2

  // Equals
  extension [A](a: Aggregate[A])
    infix def eq(b: Aggregate[A]): Aggregate[Boolean] =
      pointwise(a, b, _ == _)

  // Logic operators
  extension (a: Aggregate[Boolean])
    infix def &(b: Aggregate[Boolean]): Aggregate[Boolean] =
      pointwise(a, b, _ & _)
    infix def |(b: Aggregate[Boolean]): Aggregate[Boolean] =
      pointwise(a, b, _ | _)

  // MATH
  extension [A: Numeric as num](a: Aggregate[A])
    infix def +(b: Aggregate[A]): Aggregate[A] =
      pointwise(a, b, num.plus)
    infix def -(b: Aggregate[A]): Aggregate[A] =
      pointwise(a, b, num.minus)
    infix def *(b: Aggregate[A]): Aggregate[A] =
      pointwise(a, b, num.times)
    infix def <(b: Aggregate[A]): Aggregate[Boolean] =
      pointwise(a, b, num.lt)
    infix def >(b: Aggregate[A]): Aggregate[Boolean] =
      pointwise(a, b, num.gt)
    infix def <=(b: Aggregate[A]): Aggregate[Boolean] =
      pointwise(a, b, num.lteq)
    infix def >=(b: Aggregate[A]): Aggregate[Boolean] =
      pointwise(a, b, num.gteq)

  extension [A: Fractional as frac](a: Aggregate[A])
    infix def /(b: Aggregate[A]): Aggregate[A] =
      pointwise(a, b, frac.div)
