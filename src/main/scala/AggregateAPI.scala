package aggregate

import aggregate.NValues.*

trait AggregateAPI:
  type Device
  type Aggregate[_]
  type Env = Map[Device, ValueTree[Any]]
  type ValueTree[+A]
  extension [A](vt: ValueTree[A]) def nv: NValue[A]
  case class Input(uid: Device, sensors: Map[String, NValue[Any]])

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
    def update(d: Aggregate[Device], f: A => A): Aggregate[A]
    def run(using Env, Input): ValueTree[A]

  extension [A](fa: Aggregate[A])
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B]
    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B]

  given pureGiven[A]: Conversion[A, Aggregate[A]]
  given nvalGiven[A]: Conversion[NValue[A], Aggregate[A]]

object AggregateAPI extends AggregateAPI:
  opaque type Device = Int
  export alignment.Test.{
    Aggregate,
    sensor,
    call,
    exchange,
    nfold,
    uid,
    self,
    overrideDevice as update,
    map,
    flatMap,
    mux,
    branch
  }
  opaque type ValueTree[+A] = alignment.AlignmentModule.AlignmentTree[NValue[A]]
  extension [A](vt: ValueTree[A])
    def nv: NValue[A] =
      vt.value

  object Env:
    def apply(m: Map[Device, ValueTree[Any]]): Env = m
    def apply(vts: (Device, ValueTree[Any])*): Env = Map(vts*)

  extension [A](fa: Aggregate[A])
    def run(using env: Env, input: Input): ValueTree[A] =
      val e = alignment.AlignmentModule.Env.fromMap[Device](env)
      given Device = input.uid
      given Map[String, NValue[Any]] = input.sensors
      alignment.AlignmentModule.run(fa)(e)

  given pureGiven[A]: Conversion[A, Aggregate[A]] with
    def apply(x: A): Aggregate[A] = alignment.Test.pure(x)
  given nvalGiven[A]: Conversion[NValue[A], Aggregate[A]] with
    def apply(x: NValue[A]): Aggregate[A] = alignment.Test.pureNV(x)

  object Device:
    def fromInt(i: Int): Device = i
