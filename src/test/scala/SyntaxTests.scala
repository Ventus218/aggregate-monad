import aggregate.AggregateAPI.*
import aggregate.AggregateLib.*
import scala.language.implicitConversions

def pingPong(): NVAggregate[Int] =
  exchange(0)(n => retsend(n + 1))

def senseDist: NVAggregate[Float] =
  sensor("senseDist")

def distanceEstimate(n: NVAggregate[Float]): NVAggregate[Float] =
  nfold(Float.PositiveInfinity)(n + senseDist)(math.min)

def distanceTo(src: NVAggregate[Boolean]): NVAggregate[Float] =
  exchange(Float.PositiveInfinity)(n =>
    retsend(mux(src)(0f)(distanceEstimate(n)))
  )

def average(
    weight: NVAggregate[Float],
    value: NVAggregate[Float]
): NVAggregate[Float] =
  val totW = nfold(weight)(nbr(0f, weight))(_ + _)
  val totVl = nfold(weight * value)(nbr(0f, weight * value))(_ + _)
  totVl / totW

def closestFire(
    temperature: NVAggregate[Float],
    smoke: NVAggregate[Float]
): NVAggregate[Float] =
  val trust = nfold(1f)(1)(_ + _);
  for
    avgTemp <- average(trust, temperature).self
    hot = avgTemp > 60
    avgSmoke <- average(trust, smoke).self
    cloudy = avgTemp > 10
    res <- distanceTo(hot & cloudy)
  yield res

def distanceToGateways(
    local: NVAggregate[Boolean],
    gateway: NVAggregate[Boolean]
): NVAggregate[Float] =
  branch(local)(Float.PositiveInfinity)(distanceTo(gateway))

def distanceInServiceProvisioning(
    local: NVAggregate[Boolean],
    requester: NVAggregate[Boolean],
    gateway: NVAggregate[Boolean]
): NVAggregate[Float] =
  branch(local)(distanceTo(requester))(distanceTo(gateway))

def gossipEver(event: NVAggregate[Boolean]): NVAggregate[Boolean] =
  exchange(false)((n) =>
    retsend(nfold(n)(for
      _ <- event
      s <- n.self
      event <- event.self
    yield nv(s | event))(_ | _))
  )
