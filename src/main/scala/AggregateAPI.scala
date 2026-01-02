package aggregate

trait AggregateAPI:
  type Device
  type NValue[_]
  type Aggregate[_]
  type NVAggregate[A] = Aggregate[NValue[A]]

  def sensor[A](name: NVAggregate[String]): NVAggregate[A]
  def call[A](f: Aggregate[() => NVAggregate[A]]): NVAggregate[A]
  def exchange[A](init: NVAggregate[A])(
      f: NVAggregate[A] => (NVAggregate[A], NVAggregate[A])
  ): NVAggregate[A]

  def mux[A](cond: NVAggregate[Boolean])(th: NVAggregate[A])(
      el: NVAggregate[A]
  ): NVAggregate[A]

  // TODO: should we wrap f return type in Aggregate?
  def nfold[A, B](init: NVAggregate[A])(a: NVAggregate[B])(
      f: (A, B) => A
  ): NVAggregate[A]
  def retsend[A](a: NVAggregate[A]): (NVAggregate[A], NVAggregate[A])

  // There may be better alternatives
  def nv[A](a: A): NValue[A]

  extension [A](fa: Aggregate[A])
    def map[B](f: A => B): Aggregate[B]
    def flatMap[B](f: A => Aggregate[B]): Aggregate[B]

  extension [A](fa: NVAggregate[A]) def self: Aggregate[A]

  given pureGiven[A]: Conversion[A, Aggregate[A]]
  given pureNVGiven[A]: Conversion[A, NVAggregate[A]]

object AggregateAPI extends AggregateAPI:
  type Device = Int
  case class NValue[A]()

  override def sensor[A](name: NVAggregate[String]): NVAggregate[A] = ???

  // TODO: recheck
  override def call[A](f: Aggregate[() => NVAggregate[A]]): NVAggregate[A] = ???

  override def exchange[A](init: NVAggregate[A])(
      f: NVAggregate[A] => (NVAggregate[A], NVAggregate[A])
  ): NVAggregate[A] = ???

  override def mux[A](cond: NVAggregate[Boolean])(th: NVAggregate[A])(
      el: NVAggregate[A]
  ): NVAggregate[A] = ???

  override def nfold[A, B](init: NVAggregate[A])(a: NVAggregate[B])(
      f: (A, B) => A
  ): NVAggregate[A] = ???

  override def retsend[A](a: NVAggregate[A]): (NVAggregate[A], NVAggregate[A]) =
    ???

  def nv[A](a: A): NValue[A] = ???

  extension [A](fa: Aggregate[A])
    override def map[B](f: A => B): Aggregate[B] = ???
    override def flatMap[B](f: A => Aggregate[B]): Aggregate[B] = ???

  extension [A](fa: NVAggregate[A]) override def self: Aggregate[A] = ???

  given pureGiven[A]: Conversion[A, Aggregate[A]] = ???

  given pureNVGiven[A]: Conversion[A, NVAggregate[A]] = ???


trait AggregateLib:
  import AggregateAPI.*
  def nbr[A](default: NVAggregate[A], send: NVAggregate[A]): NVAggregate[A]
  def branch[A]( cond: NVAggregate[Boolean])( the: => NVAggregate[A])( els: => NVAggregate[A]): NVAggregate[A]

  // MATH
  extension [A: Numeric](a: NVAggregate[A])
    infix def +(b: NVAggregate[A]): NVAggregate[A]
    infix def -(b: NVAggregate[A]): NVAggregate[A]
    infix def *(b: NVAggregate[A]): NVAggregate[A]

  extension [A: Fractional](a: NVAggregate[A])
    infix def /(b: NVAggregate[A]): NVAggregate[A]

object AggregateLib extends AggregateLib:
  import AggregateAPI.*
  def nbr[A](default: NVAggregate[A], send: NVAggregate[A]): NVAggregate[A] = ???
  def branch[A]( cond: NVAggregate[Boolean])( the: => NVAggregate[A])( els: => NVAggregate[A]): NVAggregate[A] = ???

  // MATH
  extension [A: Numeric](a: NVAggregate[A])
    infix def +(b: NVAggregate[A]): NVAggregate[A] = ???
    infix def -(b: NVAggregate[A]): NVAggregate[A] = ???
    infix def *(b: NVAggregate[A]): NVAggregate[A] = ???

  extension [A: Fractional](a: NVAggregate[A])
    infix def /(b: NVAggregate[A]): NVAggregate[A] = ???
