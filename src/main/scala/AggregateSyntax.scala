package aggregate.free

object AggregateSyntax:
  import aggregate.NValues.*
  import aggregate.AggregateAPI.Device
  private enum AggregateGrammar[A]:
    case Exchange[A, S](
        default: Aggregate[S],
        body: Aggregate[S] => (Aggregate[A], Aggregate[S])
    ) extends AggregateGrammar[A]
    case NFold[A, B](init: Aggregate[A], a: Aggregate[B], f: (A, B) => A)
        extends AggregateGrammar[A]
    case Call(f: () => Aggregate[A])
    case Sensor(name: Aggregate[String])
    case Uid() extends AggregateGrammar[Device]
    case Self(a: Aggregate[A])
    // More generally this will become UpdateDevice and then we can implement this through Map
    case UpdateSelf[A, B](fa: Aggregate[A], f: A => B)
        extends AggregateGrammar[B]
    case Pure(nvalues: NValue[A])
    case Map_[A, B](a: Aggregate[A], f: A => B) extends AggregateGrammar[B]
    case PoinwiseOp[A, B](a: Aggregate[A], b: Aggregate[A], f: (A, A) => B)
        extends AggregateGrammar[B]
  import AggregateGrammar.*

  import cats.free.Free
  opaque type Aggregate[A] = Free[AggregateGrammar, A]

  object Aggregate:
    def apply[A](a: A): Aggregate[A] = Free.liftF(Pure(NValue(a)))

  extension [A](fa: Aggregate[A])
    // Re-expose map that was hidden by using an opaque type
    def map[B](f: A => B): Aggregate[B] = fa.map(f)

  def sensor[A](name: Aggregate[String]): Aggregate[A] =
    Free.liftF(Sensor(name))

  def call[A](f: () => Aggregate[A]): Aggregate[A] =
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
  import cats.data.Reader
  import cats.~>

  case class Input(
      uid: Device,
      sensors: Map[String, NValue[Any]],
      messages: Map[Device, ValueTree[Any]]
  )

  private type InputReader[A] = Reader[Input, A]
  // private enum ValueTreeState[+A]:
  //   case Empty
  //   case VT(valueTree: ValueTree[A], path: Path)
  // // TODO: check if we can add a type param B and make ValueTreeState typed
  // private type AggregateImpl[A] = StateT[InputReader, ValueTreeState[Any], A]
  //
  // extension (a: AggregateImpl[?])
  //   private def _run(input: Input): ValueTreeState[Any] =
  //     a.runS(ValueTreeState.Empty).run(input)
  //
  // private object AggregateImpl:
  //   extension [A](o: Option[A])
  //     def getOrThrowInvalidPath: A =
  //       o match
  //         case Some(value) => value
  //         case None =>
  //           throw IllegalArgumentException("tree doesn't contain path")
  //
  //   def exit(nv: NValue[?]): AggregateImpl[Unit] =
  //     StateT.modify(s =>
  //       s match
  //         case ValueTreeState.Empty => s
  //         case ValueTreeState.VT(valueTree, path) =>
  //           val vt = valueTree.setNValueAt(nv, path).getOrThrowInvalidPath
  //           ValueTreeState.VT(vt, path.exit)
  //     )
  //   def enter: AggregateImpl[Unit] =
  //     // StateT.modify(s =>
  //     //   s match
  //     //     case ValueTreeState.Empty =>
  //     //       ValueTreeState.VT(ValueTree.node(null), path.)
  //     //     case ValueTreeState.VT(valueTree, path) =>
  //     //       val vt = valueTree.setNValueAt(nv, path).getOrThrowInvalidPath
  //     //       ValueTreeState.VT(vt, path.exit)
  //     // )
  //     ???
  //
  //   def node[A](
  //       nv: NValue[A],
  //       children: Seq[ValueTree[A]] = Seq()
  //   ): AggregateImpl[Unit] =
  //     StateT.set(ValueTreeState.VT(ValueTree.node(nv, children), Path.empty))
  //
  //   def call[A](
  //       id: String,
  //       nv: NValue[A],
  //       children: Seq[ValueTree[A]] = Seq()
  //   ): AggregateImpl[Unit] =
  //     StateT.set(
  //       ValueTreeState.VT(ValueTree.call(id, nv, children), Path.empty)
  //     )
  //
  //   // def call[A](id: String, f: () => Aggregate[A]): AggregateImpl[Unit] =
  //   //   StateT.modify(s =>
  //   //     s match
  //   //       case ValueTreeState.Empty =>
  //   //         ValueTreeState.VT(ValueTree.node(nv), Path.empty)
  //   //       case ValueTreeState.VT(valueTree, path) =>
  //   //         val (newVt, newPath) =
  //   //           valueTree.appendNodeAt(nv, path).getOrThrowInvalidPath
  //   //         ValueTreeState.VT(newVt, newPath)
  //   //   )
  //
  //   def input: AggregateImpl[Input] =
  //     StateT.liftF(Reader(identity))
  //
  //   def uid: AggregateImpl[Device] =
  //     for input <- input
  //     yield input.uid
  //
  //   def messages: AggregateImpl[Map[Device, ValueTree[Any]]] =
  //     for input <- input
  //     yield input.messages
  //
  //   def sensor[A](name: Aggregate[String]): AggregateImpl[Unit] =
  //     for
  //       name <- name.self.foldMap(compiler)
  //       sensors <- StateT.liftF(Reader((_: Input).sensors))
  //       // TODO: only aligned devices??
  //       _ <- nValueNode(sensors(name))
  //     yield ()
  //
  //   // def alignedDevices: AggregateImpl[Set[Device]] =
  //   //   for
  //   //     messages <- messages
  //   //     path <- ???
  //   //   yield messages.filter((d, t) => t.subtreeAt(???))

  import cats.data.ReaderT
  import cats.data.Cont
  type ValueTreeCont[A] = Cont[ValueTree[Any], A]
  type AggregateImpl[A] = ReaderT[ValueTreeCont, Input, A]

  object AggregateImpl:
    def input: AggregateImpl[Input] =
      ReaderT.ask
    def uid: AggregateImpl[Device] =
      input.map(_.uid)
    def sensors: AggregateImpl[Map[String, NValue[Any]]] =
      input.map(_.sensors)
    def leaf(a: NValue[Any]): AggregateImpl[ValueTree[Any]] =
      ReaderT.liftF(
        Cont(next => next(ValueTree.node(a)))
      )

  private def compiler: AggregateGrammar ~> AggregateImpl =
    new (AggregateGrammar ~> AggregateImpl):
      def apply[A](fa: AggregateGrammar[A]): AggregateImpl[A] =
        import AggregateGrammar.*
        fa match
          case Exchange(default, body) => ???
          case NFold(init, a, f)       => ???
          case Call(f)                 => ???
          case Sensor(name)            => ???
          case Uid() =>
            for
              uid <- AggregateImpl.uid
              _ <- AggregateImpl.leaf(NValue(uid))
            yield uid
          case Self(a)           => ???
          case UpdateSelf(fa, f) => ???
          case Pure(nvalues) =>
            for
              input <- AggregateImpl.input
              _ <- AggregateImpl.nValueNode(nvalues)
              _ <- AggregateImpl.exit
            yield (nvalues(input.uid))
          case PoinwiseOp(a, b, f) =>
            for
              a <- a.foldMap(compiler)
              b <- b.foldMap(compiler)
            yield ???
          case Map_(a, f) => ???

  extension [A](prog: Aggregate[A])
    def myrun(input: Input): (ValueTree[Any], A) =
      val (valueTreeState, output) = prog
        .foldMap(compiler)
        .run(ValueTreeState.Empty)
        .run(input)
      valueTreeState.match
        case ValueTreeState.Empty => ??? // Program was empty???
        case ValueTreeState.VT(valueTree, path) =>
          (valueTree, output)

  // @main def main: Unit =
  //   println:
  //     uid.myrun(Input(Device.fromInt(3), Map(), Map()))

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
