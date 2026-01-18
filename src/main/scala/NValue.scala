package aggregate

object NValues:
  import aggregate.AggregateAPI.Device

  case class NValue[+A](default: A, values: Map[Device, A] = Map()):
    def apply(d: Device): A = values.get(d).getOrElse(default)
    override def toString(): String =
      val overrides = values.toList
        .filter((_, a) => a != default)
        .map((d, a) => s"$d -> $a")
        .mkString(", ")
      s"$default[$overrides]"

  extension [A](nv: NValue[A])
    def set(d: Device, value: A): NValue[A] =
      NValue(nv.default, nv.values.updated(d, value))

    def setWith(d: Device, f: A => A): NValue[A] =
      val value = f(nv(d))
      NValue(nv.default, nv.values.updated(d, value))

    def map[B](f: A => B): NValue[B] =
      NValue(f(nv.default), nv.values.view.mapValues(f).toMap)

    def flatMap[B](f: A => NValue[B]): NValue[B] =
      NValue(
        f(nv.default).default,
        (nv.values.keySet ++ f(nv.default).values.keySet)
          .map(d => (d, f(nv(d))(d)))
          .toMap
      )
