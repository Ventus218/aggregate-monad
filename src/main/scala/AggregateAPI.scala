package aggregate

import aggregate.NValues.*

trait AggregateAPI:
  type Device
  type Aggregate[_]

  def sensor[A](name: Aggregate[String]): Aggregate[A]
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A]

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A]

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A]

  // TODO: here just until we can implement call
  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A]

  def uid: Aggregate[Device]

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A]
    def updateSelf(f: A => A): Aggregate[A]

  extension [A](fa: Aggregate[A])
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B]
    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B]

  def pointwise[A, B, C](
      a: Aggregate[A],
      b: Aggregate[B],
      f: (A, B) => C
  ): Aggregate[C]

  given pureGiven[A]: Conversion[A, Aggregate[A]]
  given nvalGiven[A]: Conversion[NValue[A], Aggregate[A]]

object AggregateAPI extends AggregateAPI:
  opaque type Device = Int
  export aggregate.nonfree.AggregateImpl.{
    Aggregate,
    sensor,
    call,
    exchange,
    nfold,
    uid,
    self,
    updateSelf,
    map,
    flatMap,
    mux,
    branch,
    pointwise
  }

  given pureGiven[A]: Conversion[A, Aggregate[A]] =
    aggregate.nonfree.AggregateImpl.pureGiven
  given nvalGiven[A]: Conversion[NValue[A], Aggregate[A]] =
    aggregate.nonfree.AggregateImpl.nvalGiven

  object Device:
    def fromInt(i: Int): Device = i
