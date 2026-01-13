package aggregate

object AggregateLib :
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
