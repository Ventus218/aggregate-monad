// import aggregate.AggregateAPI.{given, *}
// import aggregate.AggregateLib.*
// import scala.language.implicitConversions
//
// def pingPong(): Aggregate[Int] =
//   exchange(0)(n => retsend(n + 1))
//
// def senseDist: Aggregate[Float] =
//   sensor("senseDist")
//
// def distanceEstimate(n: Aggregate[Float]): Aggregate[Float] =
//   nfold(Float.PositiveInfinity)(n + senseDist)(math.min)
//
// def gradient(src: Aggregate[Boolean]): Aggregate[Float] =
//   exchange(Float.PositiveInfinity)(n =>
//     retsend(mux(src)(0f)(distanceEstimate(n)))
//   )
//
// def average(
//     weight: Aggregate[Float],
//     value: Aggregate[Float]
// ): Aggregate[Float] =
//   val totW = nfold(weight)(nbr(0f, weight))(_ + _)
//   val totVl = nfold(weight * value)(nbr(0f, weight * value))(_ + _)
//   totVl / totW
//
// def closestFire(
//     temperature: Aggregate[Float],
//     smoke: Aggregate[Float]
// ): Aggregate[Float] =
//   val trust = nfold(1f)(1)(_ + _) // number of neighbours + self
//   val hot = average(trust, temperature) > 60f
//   val cloudy = average(trust, smoke) > 10f
//   gradient(hot & cloudy)
//
// def distanceToGateways(
//     local: Aggregate[Boolean],
//     gateway: Aggregate[Boolean]
// ): Aggregate[Float] =
//   branch(local)(Float.PositiveInfinity)(gradient(gateway))
//
// def distanceInServiceProvisioning(
//     local: Aggregate[Boolean],
//     requester: Aggregate[Boolean],
//     gateway: Aggregate[Boolean]
// ): Aggregate[Float] =
//   branch(local)(gradient(requester))(gradient(gateway))
//
// def gossipEver(event: Aggregate[Boolean]): Aggregate[Boolean] =
//   exchange(false)((n) => retsend(nfold(n.self | event)(n)(_ | _)))
//
// def broadcast[A](dist: Aggregate[Float], value: Aggregate[A]): Aggregate[A] =
//   extension [A](a: Aggregate[A])
//     infix def ->[B](b: Aggregate[B]): Aggregate[(A, B)] =
//       pointwise(a, b, (a, b) => (a, b))
//   extension [A, B](a: Aggregate[(A, B)])
//     def _1: Aggregate[A] = a.map(_.map(_._1))
//     def _2: Aggregate[B] = a.map(_.map(_._2))
//
//   val loc = (dist -> value)
//   exchange(loc)(n =>
//     val minDistAndValue =
//       nfold(init = loc)(n)((t1, t2) => if t1._1 <= t2._1 then t1 else t2)
//     retsend:
//       (dist -> minDistAndValue._2)
//   )._2
//
// def dilate(
//     region: Aggregate[Boolean],
//     width: Aggregate[Float]
// ): Aggregate[Boolean] =
//   gradient(region) < width
//
// def distance(
//     from: Aggregate[Boolean],
//     to: Aggregate[Boolean]
// ): Aggregate[Float] =
//   broadcast(gradient(from), gradient(to))
//
// def channel(
//     source: Aggregate[Boolean],
//     dest: Aggregate[Boolean],
//     width: Aggregate[Float]
// ): Aggregate[Boolean] =
//   dilate(gradient(source) + gradient(dest) <= distance(source, dest), width)
