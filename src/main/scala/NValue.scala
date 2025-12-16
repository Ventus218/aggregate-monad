case class NValue[A](default: A, values: Map[Device, A]):
  def map[B](f: A => B): NValue[B] =
    NValue(f(default), values.view.mapValues(f).toMap)

given [A]: Conversion[A, NValue[A]] with
  def apply(x: A): NValue[A] = NValue(x, Map())
