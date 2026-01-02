import aggregate.AggregateAPI.*
import aggregate.AggregateLib.*
import scala.language.implicitConversions

def pingPong(): Aggregate[Int] =
  exchange(0)(n => retsend(n + 1))

def senseDist: Aggregate[Float] =
  sensor("senseDist")

def distanceEstimate(n: Aggregate[Float]): Aggregate[Float] =
  nfold(Float.PositiveInfinity)(n + senseDist)(math.min)

def gradient(src: Aggregate[Boolean]): Aggregate[Float] =
  exchange(Float.PositiveInfinity)(n =>
    retsend(mux(src)(0f)(distanceEstimate(n)))
  )

def average(
    weight: Aggregate[Float],
    value: Aggregate[Float]
): Aggregate[Float] =
  // TODO: is this safe? what if i receive message from a device
  // in the first nbr but don't receive any in the second nbr ??
  val totW = nfold(weight)(nbr(0f, weight))(_ + _)
  val totVl = nfold(weight * value)(nbr(0f, weight * value))(_ + _)
  totVl / totW

def closestFire(
    temperature: Aggregate[Float],
    smoke: Aggregate[Float]
): Aggregate[Float] =
  val trust = nfold(1f)(1)(_ + _) // number of neighbours + self
  val hot = average(trust, temperature) > 60f
  val cloudy = average(trust, smoke) > 10f
  gradient(hot & cloudy)

def distanceToGateways(
    local: Aggregate[Boolean],
    gateway: Aggregate[Boolean]
): Aggregate[Float] =
  branch(local)(Float.PositiveInfinity)(gradient(gateway))

def distanceInServiceProvisioning(
    local: Aggregate[Boolean],
    requester: Aggregate[Boolean],
    gateway: Aggregate[Boolean]
): Aggregate[Float] =
  branch(local)(gradient(requester))(gradient(gateway))

def gossipEver(event: Aggregate[Boolean]): Aggregate[Boolean] =
  exchange(false)((n) => retsend(nfold(n.self | event)(n)(_ | _)))

def dilate(
    region: Aggregate[Boolean],
    width: Aggregate[Float]
): Aggregate[Boolean] =
  for
    dist <- gradient(region)
    width <- width
  yield dist < width

def distance(
    from: Aggregate[Boolean],
    to: Aggregate[Boolean]
): Aggregate[Float] =
  ???

def channel(
    source: Aggregate[Boolean],
    dest: Aggregate[Boolean],
    width: Aggregate[Float]
): Aggregate[Boolean] =
  dilate(gradient(source) + gradient(dest) <= distance(source, dest), width)
