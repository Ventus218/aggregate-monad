package aggregate.free

import aggregate.AggregateAPI.Device
import orderedtree.OrderedTree.LeftToRightOrderedTreeBuilder
import aggregate.free.AggregateAlignment.Alignment.alignedMessages

object AggregateSyntax:
  private enum AggregateGrammar[A]:
    case Exchange[A, S](
        default: Aggregate[S],
        body: Aggregate[S] => (Aggregate[A], Aggregate[S])
    ) extends AggregateGrammar[A]
    case NFold[A, B](init: Aggregate[A], a: Aggregate[B], f: (A, B) => A)
        extends AggregateGrammar[A]
    case Call(f: Aggregate[() => Aggregate[A]])
    case Sensor(name: Aggregate[String])
    case Uid() extends AggregateGrammar[Device]
    case Self(a: Aggregate[A])
    // More generally this will become UpdateDevice and then we can implement this through Map
    case UpdateSelf[A, B](fa: Aggregate[A], f: A => B)
        extends AggregateGrammar[B]
    case Pure(nvalues: NValue[A])
    case Map_[A, B](a: Aggregate[A], f: A => B) extends AggregateGrammar[B]
  import AggregateGrammar.*

  import cats.free.Free
  opaque type Aggregate[A] = Free[AggregateGrammar, A]

  object Aggregate:
    def apply[A](a: A): Aggregate[A] = Free.liftF(Pure(NValue(a)))

  extension [A](fa: Aggregate[A])
    // Re-expose flatMap and map that were hidden by using an opaque type
    def map[B](f: A => B): Aggregate[B] = fa.map(f)
    def flatMap[B](f: A => Aggregate[B]): Aggregate[B] = fa.flatMap(f)

  def sensor[A](name: Aggregate[String]): Aggregate[A] =
    Free.liftF(Sensor(name))

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] =
    Free.liftF(Call(f))

  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] =
    Free.liftF(Exchange(default, f))

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] =
    Free.liftF(NFold(init, a, f))

  def uid: Aggregate[Device] =
    Free.liftF(Uid())

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A] = Free.liftF(Self(fa))
    def updateSelf(f: A => A): Aggregate[A] = Free.liftF(UpdateSelf(fa, f))

  import AggregateAlignment.*
  import cats.data.StateT
  import cats.~>

  private case class NValue[+A](default: A, values: Map[Device, A] = Map())
  private type Compiled[A] =
    StateT[Alignment, LeftToRightOrderedTreeBuilder[NValue[Any]], A]
  private object Compiled:
    def enter[A](nvalue: NValue[A]): Compiled[Unit] =
      StateT.modify(_.enter(nvalue))
    def exit: Compiled[Unit] =
      StateT.modify(_.exit)

  private def compiler: AggregateGrammar ~> Compiled =
    new (AggregateGrammar ~> Compiled):
      def apply[A](fa: AggregateGrammar[A]): Compiled[A] =
        import AggregateGrammar.*
        fa match
          case Exchange(default, body) =>
            for
              default <- default.self.foldMap(compiler)
              alignedMessages <- StateT.liftF(
                Alignment.alignedMessages[default.type]
              )
              res = body(
                Free
                  .liftF(Pure(NValue(default, alignedMessages)))
              )
              ret <- res._1.foldMap(compiler)
              send <- res._2.foldMap(compiler)
            yield ret
          case NFold(init, a, f) =>
            for
              init <- init.self.foldMap(compiler)
              a <- a.foldMap(compiler)
              messages <- StateT.liftF(Alignment.alignedMessages[a.type])
            yield messages.values.foldLeft(init)((acc, elem) => f(acc, elem))
          case Call(f)           => ???
          case UpdateSelf(fa, f) => ???
          case Self(a)           => ???
          case Uid()             => StateT.liftF(Alignment.uid)
          case Sensor(name) =>
            for
              name <- name.self.foldMap(compiler)
              res <- StateT.liftF(Alignment.sensor(name))
            yield res.asInstanceOf[A]
          case Pure(nvalue) =>
            for
              _ <- Compiled.enter(nvalue)
              _ <- Compiled.exit
            yield ???
          case Map_(a, f) =>
            for
              a <- a.foldMap(compiler)
              messages <- StateT.liftF(Alignment.alignedMessages[a.type])
              mapped = messages.view.mapValues(f)
            yield ???
  extension [A](prog: Aggregate[A])
    def run(
        uid: Device,
        sensors: Map[String, Any],
        messages: Map[Device, AlignmentTree[Any]]
    ) =
      val sos = prog
        .foldMap(compiler)
        // TODO: null is ugly
        .runA(LeftToRightOrderedTreeBuilder.root(NValue(null)))
        .run(uid, sensors, messages)
//
// object Utils:
//   import AggregateSyntax.*
//   extension [A](a: Aggregate[A])
//     def self: Agg[A] =
//       for
//         mid <- mid
//         a <- a
//       yield a(mid)
//
//   def mux[A](
//       cond: Aggregate[Boolean],
//       the: Aggregate[A],
//       els: Aggregate[A]
//   ): Aggregate[A] =
//     for
//       cond <- cond.self
//       the <- the
//       els <- els
//     yield if cond then the else els
//
//   def branch[A](
//       cond: Aggregate[Boolean],
//       the: => Aggregate[A],
//       els: => Aggregate[A]
//   ): Aggregate[A] =
//     for
//       cond <- cond.self
//       res <- if cond then the else els
//     yield res
//
//   def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A] =
//     exchange(default, (n) => (n, send))
//
//   def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A]) = (a, a)
//
//   // TODO: unfortunately we cannot provice a given instance for math.Numeric
//   // since some functions return pure types.
//   // Example: compare[A](x: Aggregate[A], y: Aggregate[A]): Int
//   // This would force us to "exit" from the monad
//
//   private def evaluateAndApply[A, B](a: Aggregate[A], b: Aggregate[A])(
//       f: (NValue[A], NValue[A]) => NValue[B]
//   ): Aggregate[B] =
//     for
//       a <- a
//       b <- b
//     yield f(a, b)
//
//   private def nvNum[A: Numeric] = summon[Numeric[NValue[A]]]
//   extension [A: Numeric](a: Aggregate[A])
//     infix def +(b: Aggregate[A]): Aggregate[A] =
//       evaluateAndApply(a, b)(nvNum.plus)
//     infix def -(b: Aggregate[A]): Aggregate[A] =
//       evaluateAndApply(a, b)(nvNum.minus)
//     infix def *(b: Aggregate[A]): Aggregate[A] =
//       evaluateAndApply(a, b)(nvNum.times)
//
//   private def nvFrac[A: Fractional] = summon[Fractional[NValue[A]]]
//   extension [A: Fractional](a: Aggregate[A])
//     infix def /(b: Aggregate[A]): Aggregate[A] =
//       evaluateAndApply(a, b)(nvFrac.div)
