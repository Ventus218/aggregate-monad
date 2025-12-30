import NValues.*

type Device = Int

// trait AggregateAPI:
//   type Aggregate[_]
//
//   def sensor[A](name: String): Aggregate[NValue[A]]
//   def call[A](f: Aggregate[() => Aggregate[NValue[A]]]): Aggregate[NValue[A]]
//   def exchange[A](init: Aggregate[NValue[A]])
//   (f: NValue[A] => (Aggregate[NValue[A]], Aggregate[NValue[A]])): Aggregate[NValue[A]]
//
//   def mux[A](cond: Aggregate[NValue[Boolean]])(th: Aggregate[NValue[A]])(el: Aggregate[NValue[A]]): Aggregate[NValue[A]]
//
//   def nfold[A](init: NValue[A])(op: (NValue[A], NValue[A]) => NValue[A])(a: NValue[A]): NValue[A]
//   def retsend[A](a: NValue[A]): (NValue[A], NValue[A])
//
//   extension [A](fa: Aggregate[A])
//     def map[ B](f: A => B): Aggregate[B]
//     def flatMap[ B](f: A => Aggregate[B]): Aggregate[B]
//
//   given cats.Monad[Aggregate]
//
//   given pureGiven [A]:Conversion[A, Aggregate[A]]
//
//
// object AggregateAPI extends AggregateAPI
