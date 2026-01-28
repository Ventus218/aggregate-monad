package aggregate

import NValues.*
import ValueTrees.*

trait AggregateAPI:
  type Device
  type Aggregate[_]
  type Env = Map[Device, ValueTree[Any]]
  case class Input(uid: Device)

  def sensor[A](s: => Aggregate[A]): Aggregate[A]
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A]

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A]

  def uid: Aggregate[Device]

  extension [A](fa: Aggregate[A])
    def update(d: Aggregate[Device], f: A => A): Aggregate[A]
    def run(using Env, Input): ValueTree[A]

  extension [A](fa: Aggregate[A])
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B]
    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B]

  given pureGiven[A]: Conversion[A, Aggregate[A]]
  given nvalGiven[A]: Conversion[NValue[A], Aggregate[A]]

object AggregateAPI extends AggregateAPI:
  import aggregate.AggregateImpl as impl

  opaque type Device = Int
  opaque type Aggregate[+A] = impl.Aggregate[A]

  def sensor[A](s: => Aggregate[A]): Aggregate[A] = impl.sensor(s)

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = impl.call(f)

  def exchange[R, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[R], Aggregate[S])
  ): Aggregate[R] = impl.exchange(default)(f)

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] = impl.nfold(init)(a)(f)

  def uid: Aggregate[Device] = impl.uid

  extension [A](fa: Aggregate[A])
    def update(d: Aggregate[Device], f: A => A): Aggregate[A] =
      impl.update(fa)(d, f)

    def run(using Env, Input): ValueTree[A] = impl.run(fa)

    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] =
      impl.map(fa)(f)

    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B] =
      impl.flatMap(fa)(f)

  object Env:
    def apply(env: Map[Device, ValueTree[Any]]): Env = env
    def apply(env: (Device, ValueTree[Any])*): Env = Map(env*)

  given pureGiven[A]: Conversion[A, Aggregate[A]] with
    def apply(x: A): Aggregate[A] = impl.pure(x)

  given nvalGiven[A]: Conversion[NValue[A], Aggregate[A]] with
    def apply(x: NValue[A]): Aggregate[A] = impl.pure(x)

  object Device:
    def fromInt(i: Int): Device = i
