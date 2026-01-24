package aggregate

import aggregate.NValues.*

trait AggregateAPI:
  type Device
  type Aggregate[+_]
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
  opaque type Aggregate[+A] = alignment.Test.Aggregate[A]
  import alignment.Test as al

  def sensor[A](name: Aggregate[String]): Aggregate[A] = al.sensor(name)
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = al.call(f)
  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] = al.exchange(default)(f)
  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] = al.nfold(init)(a)(f)
  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] = al.mux(cond)(th)(el)
  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] = al.branch(cond)(th)(el)
  def uid: Aggregate[Device] = al.uid

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A] = al.self(fa)
    def update(d: Aggregate[Device], f: A => A): Aggregate[A] =
      al.overrideDevice(fa)(d, f)
    // def run(using Env, Input): ValueTree[A] = al.run(fa)
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] = al.map(fa)(f)
    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B] =
      al.flatMap(fa)(f)
    def run(using env: Env, input: Input): ValueTree[A] =
      given alignment.AlignmentModule.Env[Device] =
        alignment.AlignmentModule.Env.fromMap(env.toMap)
      al.runAggregate(fa)

  opaque type ValueTree[+A] = alignment.AlignmentModule.AlignmentTree[NValue[A]]
  extension [A](vt: ValueTree[A])
    def nv: NValue[A] =
      vt.value

  object Env:
    def apply(m: Map[Device, ValueTree[Any]]): Env = m
    def apply(vts: (Device, ValueTree[Any])*): Env = Map(vts*)

  given pureGiven[A]: Conversion[A, Aggregate[A]] with
    def apply(x: A): Aggregate[A] = al.pure(x)
  given nvalGiven[A]: Conversion[NValue[A], Aggregate[A]] with
    def apply(x: NValue[A]): Aggregate[A] = al.pureNV(x)

  object Device:
    def fromInt(i: Int): Device = i
