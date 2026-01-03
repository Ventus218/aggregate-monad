package aggregate

trait AggregateAPI:
  type Device
  type Aggregate[_]

  def sensor[A](name: Aggregate[String]): Aggregate[A]
  // TODO: recheck
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A](init: Aggregate[A])(
      f: Aggregate[A] => (Aggregate[A], Aggregate[A])
  ): Aggregate[A]

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A]

  // TODO: should we wrap f return type in Aggregate?
  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A]
  def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A])

  def uid: Aggregate[Device]

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A]
    def updateSelf(f: A => A): Aggregate[A]

  extension [A](fa: Aggregate[A])
    def map[B](f: A => B): Aggregate[B]
    def flatMap[B](f: A => Aggregate[B]): Aggregate[B]

  given pureGiven[A]: Conversion[A, Aggregate[A]]

  // TODO: define equals

object AggregateAPI extends AggregateAPI:
  type Device = Int

  def sensor[A](name: Aggregate[String]): Aggregate[A] = ???

  // TODO: recheck
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = ???

  def exchange[A](init: Aggregate[A])(
      f: Aggregate[A] => (Aggregate[A], Aggregate[A])
  ): Aggregate[A] = ???

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] = ???

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] = ???

  def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A]) =
    ???

  def uid: Aggregate[Device] = ???

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A] = ???
    def updateSelf(f: A => A): Aggregate[A] = ???

  extension [A](fa: Aggregate[A])
    def map[B](f: A => B): Aggregate[B] = ???
    def flatMap[B](f: A => Aggregate[B]): Aggregate[B] = ???

  given pureGiven[A]: Conversion[A, Aggregate[A]] = ???

trait AggregateLib:
  import AggregateAPI.*
  def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A]
  def branch[A](cond: Aggregate[Boolean])(the: => Aggregate[A])(
      els: => Aggregate[A]
  ): Aggregate[A]

  extension [A, B](a: (Aggregate[A], Aggregate[B]))
    def fst: Aggregate[A]
    def snd: Aggregate[B]

  // Equals
  extension [A](a: Aggregate[A])
    infix def eq(b: Aggregate[A]): Aggregate[Boolean]

  // Logic operators
  extension (a: Aggregate[Boolean])
    infix def &(b: Aggregate[Boolean]): Aggregate[Boolean]
    infix def |(b: Aggregate[Boolean]): Aggregate[Boolean]

  // MATH
  extension [A: Numeric](a: Aggregate[A])
    infix def +(b: Aggregate[A]): Aggregate[A]
    infix def -(b: Aggregate[A]): Aggregate[A]
    infix def *(b: Aggregate[A]): Aggregate[A]
    infix def <(b: Aggregate[A]): Aggregate[Boolean]
    infix def >(b: Aggregate[A]): Aggregate[Boolean]
    infix def <=(b: Aggregate[A]): Aggregate[Boolean]
    infix def >=(b: Aggregate[A]): Aggregate[Boolean]

  extension [A: Fractional](a: Aggregate[A])
    infix def /(b: Aggregate[A]): Aggregate[A]

object AggregateLib extends AggregateLib:
  import AggregateAPI.*
  def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A] = ???
  def branch[A](cond: Aggregate[Boolean])(the: => Aggregate[A])(
      els: => Aggregate[A]
  ): Aggregate[A] = ???

  extension [A, B](a: (Aggregate[A], Aggregate[B]))
    def fst: Aggregate[A] = ???
    def snd: Aggregate[B] = ???

  // Equals
  extension [A](a: Aggregate[A])
    infix def eq(b: Aggregate[A]): Aggregate[Boolean] = ???

  // Logic operators
  extension (a: Aggregate[Boolean])
    infix def &(b: Aggregate[Boolean]): Aggregate[Boolean] = ???
    infix def |(b: Aggregate[Boolean]): Aggregate[Boolean] = ???

  // MATH
  extension [A: Numeric](a: Aggregate[A])
    infix def +(b: Aggregate[A]): Aggregate[A] = ???
    infix def -(b: Aggregate[A]): Aggregate[A] = ???
    infix def *(b: Aggregate[A]): Aggregate[A] = ???
    infix def <(b: Aggregate[A]): Aggregate[Boolean] = ???
    infix def >(b: Aggregate[A]): Aggregate[Boolean] = ???
    infix def <=(b: Aggregate[A]): Aggregate[Boolean] = ???
    infix def >=(b: Aggregate[A]): Aggregate[Boolean] = ???

  extension [A: Fractional](a: Aggregate[A])
    infix def /(b: Aggregate[A]): Aggregate[A] = ???
