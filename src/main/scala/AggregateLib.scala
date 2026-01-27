package aggregate

object AggregateLib:
  import AggregateAPI.*

  def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A] =
    exchange(default)(a => (a, send))

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

  // TODO: restore once call can be implemented
  // def branch[A](cond: Aggregate[Boolean])(the: => Aggregate[A])(
  //     els: => Aggregate[A]
  // ): Aggregate[A] =
  //   import AggregateAPI.pureGiven
  //   call:
  //     mux(cond)(pureGiven(() => the))(pureGiven(() => els))
  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] =
    import AggregateAPI.pureGiven
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
