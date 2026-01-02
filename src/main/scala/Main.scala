// import NValues.*
// import AggregateSyntax.*
// import scala.language.implicitConversions
// import scala.math.Numeric.Implicits.infixNumericOps
//
// object Main extends App:
//   def senseDist: Aggregate[Float] =
//     sensor("senseDist")
//
//   def isSource: Aggregate[Boolean] =
//     sensor("source")
//
//   def distanceEstimate(n: Aggregate[Float]): Aggregate[Float] =
//     nfold(Float.PositiveInfinity)(n + senseDist)(math.min)
//
//   def distanceTo(src: Aggregate[Boolean]): Aggregate[Float] =
//     exchange(
//       Float.PositiveInfinity,
//       (n) => retsend(mux(src, 0f, distanceEstimate(n)))
//     )
//
//   def average(
//       weight: Aggregate[Float],
//       value: Aggregate[Float]
//   ): Aggregate[Float] =
//     val totW = nfold(weight)(nbr(0f, weight))(_ + _)
//     val totVl =
//       nfold(weight * value)(nbr(0f, weight * value))(_ + _)
//     import scala.math.Fractional.Implicits.*
//     totVl / totW
//
//   def program: Aggregate[Float] =
//     average(0f, 0f) + distanceTo(isSource)
//
//   // def recursion(n: Int, default: Float): Aggregate[Float] =
//   //   for
//   //     a <- nbr(0f, 4f)
//   //     res <-
//   //       if n > 0 then recursion(n - 1, default)
//   //       else
//   //         nbr(default, default).map(nvalues =>
//   //           nfold[Float, Float](0)(plus)( nvalues)
//   //         )
//   //   yield (res)
