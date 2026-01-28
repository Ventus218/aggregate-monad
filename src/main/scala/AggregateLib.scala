package aggregate

object AggregateLib:
  import AggregateAPI.*

  def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A] =
    exchange(default)(a => (a, send))

  def nbr[A](a: Aggregate[A]): Aggregate[A] =
    exchange(a)(n => (n, a))

  def rep[A](a: Aggregate[A])(f: Aggregate[A] => Aggregate[A]): Aggregate[A] =
    exchange(a)(n => retsend(f(n.self)))

  def counter(initial: Aggregate[Int]): Aggregate[Int] =
    import scala.language.implicitConversions
    rep(initial)(_ + 1)

  def pointwise[A, B, C](
      a: Aggregate[A],
      b: Aggregate[B],
      f: (A, B) => C
  ): Aggregate[C] =
    for
      a <- a
      b <- b
    yield (for
      a <- a
      b <- b
    yield f(a, b))

  def branch[A](cond: Aggregate[Boolean])(th: => Aggregate[A])(
      el: => Aggregate[A]
  ): Aggregate[A] =
    import AggregateAPI.pureGiven
    call:
      mux(cond)(pureGiven(() => th))(pureGiven(() => el))

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] =
    import NValues.*
    for
      condNV <- cond.self
      thNV <- th
      elNV <- el
    yield (for
      condV <- condNV
      thV <- thNV
      elV <- elNV
    yield if condV then thV else elV)

  def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A]) =
    (a, a)

  extension [A, B](a: (Aggregate[A], Aggregate[B]))
    def fst: Aggregate[A] = a._1
    def snd: Aggregate[B] = a._2

  extension [A](a: Aggregate[A])
    def updateSelf(f: A => A): Aggregate[A] =
      a.update(uid, f)
    def updateSelf(b: A): Aggregate[A] =
      a.update(uid, _ => b)
    def self: Aggregate[A] =
      for
        aNV <- a
        uidNV <- uid
      yield uidNV.map(uidValue => aNV(uidValue))

  def gradient(
      src: Aggregate[Boolean]
  )(using range: Aggregate[Double]): Aggregate[Double] =
    import scala.language.implicitConversions
    exchange(Double.PositiveInfinity): v =>
      retsend:
        mux(src)(0.0):
          nfold(Double.PositiveInfinity)(range + v)(_ min _)

  def broadcast[A](src: Aggregate[Boolean])(field: Aggregate[A])(using
      range: Aggregate[Double]
  ): Aggregate[(Double, A)] =
    import scala.language.implicitConversions
    extension [A](a: Aggregate[A])
      infix def ->[B](b: Aggregate[B]): Aggregate[(A, B)] =
        pointwise(a, b, (a, b) => (a, b))
    extension [A, B](a: Aggregate[(A, B)])
      def _1: Aggregate[A] = a.map(_.map(_._1))
      def _2: Aggregate[B] = a.map(_.map(_._2))
    extension [A](dv: (Double, A))
      infix def min(dv2: (Double, A)): (Double, A) =
        if dv._1 < dv2._1 then dv else dv2

    exchange(Double.PositiveInfinity -> field): dv =>
      retsend:
        mux(src)(0.0 -> field):
          val dv1PlusRange = dv._1 + range
          val tuple = dv1PlusRange -> dv._2
          nfold(Double.PositiveInfinity -> field)(tuple)(_ min _)

  def distanceBetween(src: Aggregate[Boolean], dest: Aggregate[Boolean])(using
      range: Aggregate[Double]
  ): Aggregate[Double] =
    broadcast(src)(gradient(dest)).map(_.map(_._2))

  def channel(
      src: Aggregate[Boolean],
      dest: Aggregate[Boolean],
      width: Aggregate[Double]
  )(using range: Aggregate[Double]): Aggregate[Boolean] =
    gradient(src) + gradient(dest) - distanceBetween(src, dest) < width

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
