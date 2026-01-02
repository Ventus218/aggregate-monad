// package aggregate
//
// import scala.math.Numeric
// import scala.math.Fractional
//
// object NValues:
//
//   opaque type NValue[A] = NValueImpl[A]
//
//   private case class NValueImpl[A](default: A, values: Map[Device, A])
//
//   object NValue:
//     def apply[A](default: A, values: Map[Device, A] = Map()): NValue[A] =
//       NValueImpl(default, values)
//
//   extension [A](n: NValue[A])
//     def apply(d: Device): A =
//       n.values.get(d).getOrElse(n.default)
//     def map[B](f: A => B): NValue[B] =
//       NValue(f(n.default), n.values.view.mapValues(f).toMap)
//     def default: A =
//       n.default
//     def values: Map[Device, A] =
//       n.values
//
//   // given [A]: Conversion[A, NValue[A]] with
//   //   def apply(x: A): NValue[A] = NValue(x)
//
//   given [A: Fractional]: Fractional[NValue[A]] = ???
//   given [A: Numeric]: Numeric[NValue[A]] = ???
