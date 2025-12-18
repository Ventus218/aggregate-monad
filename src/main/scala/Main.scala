import NValues.*
import scala.language.implicitConversions

object Main:
  def distanceEstimate(n: NValue[Float]): Aggregate[Float] =
    // TODO: replace map +1 with sensor reading
    nfold(math.min, n.map(1.+), Float.PositiveInfinity)

  def distanceTo(src: Boolean): Aggregate[Float] =
    for
      n <- exchange(
        Float.PositiveInfinity,
        (n) => (n, n)
      )
      res <- mux(src, 0f, distanceEstimate(n))
    yield res

  def average(weight: Float, value: Float): Aggregate[Float] =
    for
      totW <- nfold[Float, Float](plus, nbr(0f, weight), weight)
      totVl <- nfold[Float, Float](
        plus,
        nbr(0f, weight * value),
        weight * value
      )
    yield (totVl / totW)

  def program: Aggregate[Float] =
    for
      avg <- average(0, 0)
      dst <- distanceTo(true)
    yield avg + dst
