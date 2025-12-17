import NValues.*

object Main:
  def distanceEstimate(n: NValue[Float]): Float =
    // TODO: replace map +1 with sensor reading
    nfold(math.min, n.map(1.+), Float.PositiveInfinity)

  def distanceTo(src: Boolean): Aggregate[Float] =
    exchange(
      Float.PositiveInfinity,
      (n) => retsend(mux(src, 0, distanceEstimate(n)))
    )

  def average(weight: Float, value: Float): Aggregate[Float] =
    for
      nweights <- nbr(0f, weight)
      totW = nfold[Float, Float](plus, nweights, weight)
      nvalues <- nbr(0f, weight * value)
      totVl = nfold[Float, Float](plus, nvalues, weight * value)
    yield (totVl / totW)

  def program: Aggregate[Float] =
    for
      avg <- average(0, 0)
      dst <- distanceTo(true)
    yield avg + dst

  def recursion(n: Int, default: Float): Aggregate[Float] =
    for
      a <- nbr(0f, 4f)
      res <-
        if n > 0 then recursion(n - 1, default)
        else
          nbr(default, default).map(nvalues =>
            nfold[Float, Float](plus, nvalues, 0)
          )
    yield (res)
