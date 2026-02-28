package aggregate

object NValues:
  import aggregate.AggregateAPI.Device

  opaque type NValue[+A] = NValueImpl[A]
  private case class NValueImpl[+A](default: A, values: Map[Device, A] = Map()):
    override def toString(): String =
      val overrides = values.toList
        .filter((_, a) => a != default)
        .map((d, a) => s"$d -> $a")
        .mkString(", ")
      s"$default[$overrides]"

  object NValue:
    def apply[A](default: A, values: Map[Device, A] = Map()): NValue[A] =
      NValueImpl(default, values.filter((_, a) => a != default))
    def apply[A](default: A, values: (Device, A)*): NValue[A] =
      NValue(default, values.toMap)

  extension [A](nv: NValue[A])
    def apply(d: Device): A = nv.values.get(d).getOrElse(nv.default)

    def set(d: Device, value: A): NValue[A] =
      NValue(nv.default, nv.values.updated(d, value))

    def setWith(d: Device, f: A => A): NValue[A] =
      val value = f(nv(d))
      NValue(nv.default, nv.values.updated(d, value))

    def map[B](f: A => B): NValue[B] =
      NValue(f(nv.default), nv.values.view.mapValues(f).toMap)

    def flatMap[B](f: A => NValue[B]): NValue[B] =
      val fd = f(nv.default)
      val keys = nv.values.keySet ++ fd.values.keySet
      NValue(
        fd.default,
        keys.map(d => (d, f(nv(d))(d))).toMap
      )

  def pointwise[A, B, C](
      a: NValue[A],
      b: NValue[B],
      f: (A, B) => C
  ): NValue[C] =
    for
      a <- a
      b <- b
    yield f(a, b)

  // Equals
  extension [A](a: NValue[A])
    infix def eq(b: NValue[A]): NValue[Boolean] =
      pointwise(a, b, _ == _)

  // Logic operators
  extension (a: NValue[Boolean])
    infix def &(b: NValue[Boolean]): NValue[Boolean] =
      pointwise(a, b, _ & _)
    infix def |(b: NValue[Boolean]): NValue[Boolean] =
      pointwise(a, b, _ | _)

  // MATH
  extension [A: Numeric as num](a: NValue[A])
    infix def +(b: NValue[A]): NValue[A] =
      pointwise(a, b, num.plus)
    infix def -(b: NValue[A]): NValue[A] =
      pointwise(a, b, num.minus)
    infix def *(b: NValue[A]): NValue[A] =
      pointwise(a, b, num.times)
    infix def <(b: NValue[A]): NValue[Boolean] =
      pointwise(a, b, num.lt)
    infix def >(b: NValue[A]): NValue[Boolean] =
      pointwise(a, b, num.gt)
    infix def <=(b: NValue[A]): NValue[Boolean] =
      pointwise(a, b, num.lteq)
    infix def >=(b: NValue[A]): NValue[Boolean] =
      pointwise(a, b, num.gteq)

  extension [A: Fractional as frac](a: NValue[A])
    infix def /(b: NValue[A]): NValue[A] =
      pointwise(a, b, frac.div)
