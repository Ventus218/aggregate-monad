package aggregate

object AggregateAlignment:
  import AggregateAPI.Device
  import orderedtree.OrderedTree.*

  enum ValueTreeNode[A]:
    case Val(values: Map[Device, A])
    case Exchange(values: Map[Device, A])
    case Call(id: String)

  type ValueTree[A] = OrderedTree[Option[ValueTreeNode[A]]]

  enum AlignmentGrammar[A]:
    case Enter() extends AlignmentGrammar[Unit]
    case Exit(a: Option[ValueTreeNode[Any]]) extends AlignmentGrammar[Unit]
    case CurrentCursor() extends AlignmentGrammar[Cursor]
    case AlignedDevices() extends AlignmentGrammar[Set[Device]]
    case Input() extends AlignmentGrammar[Map[Device, ValueTree[Any]]]

  // import cats.free.FreeT
  // import cats.data.Reader
  // type ValueTreesReader[A] = Reader[Map[Device, ValueTree[Any]], A]
  // type Alignment[A] = FreeT[AlignmentGrammar, ValueTreesReader, A]
  import cats.free.Free
  type Alignment[A] = Free[AlignmentGrammar, A]

  /** Creates and enters a new node with value a */
  def enter: Alignment[Unit] =
    Free.liftF(AlignmentGrammar.Enter())

  /** Exits a node setting its value to a */
  def exit(a: ValueTreeNode[Any]): Alignment[Unit] =
    Free.liftF(AlignmentGrammar.Exit(Some(a)))

  /** Exits a node */
  def exit: Alignment[Unit] =
    Free.liftF(AlignmentGrammar.Exit(None))

  def currentCursor: Alignment[Cursor] =
    Free.liftF(AlignmentGrammar.CurrentCursor())

  /** Gets all the devices currently aligned */
  def alignedDevices: Alignment[Set[Device]] =
    Free.liftF(AlignmentGrammar.AlignedDevices())

  /** Gets all the messages of aligned devices */
  def alignedMessages: Alignment[Map[Device, Any]] =
    for messages <- Free.liftF(AlignmentGrammar.Input())
      cursor <- currentCursor
      tree <- 
    yield messages.filter((d, t) => 
      cursor.path.toList match
        case Nil => 
        case h :: t => 

      
      
        ???
        ).toMap

  import cats.~>
  import cats.data.State

  private case class ProgramState(
      input: Map[Device, ValueTree[Any]],
      treeBuilder: LeftToRightOrderedTreeBuilder[Option[ValueTreeNode[Any]]]
  )
  private type TreeState[A] = State[ProgramState, A]
  private object TreeState:
    def enter: TreeState[Unit] =
      State.modify(s => s.copy(treeBuilder = s.treeBuilder.enter(None)))
    def exit(a: Option[ValueTreeNode[Any]]): TreeState[Unit] =
      State.modify(s => s.copy(treeBuilder = s.treeBuilder.exit(a)))

  private def compiler: AlignmentGrammar ~> TreeState =
    new (AlignmentGrammar ~> TreeState):
      def apply[A](fa: AlignmentGrammar[A]): TreeState[A] =
        import AlignmentGrammar.*
        fa match
          case Enter() => TreeState.enter
          case Exit(a) => TreeState.exit(a)
          case AlignedDevices() =>
            State.inspect(s => s.input.filter((d, tree) => ???).keySet)

  def run(
      prog: Alignment[?],
      input: Map[Device, ValueTree[Any]]
  ): ValueTree[Any] =
    val initProgramState = ProgramState(
      input,
      LeftToRightOrderedTreeBuilder.root(Some(ValueTreeNode.Call("root")))
    )
    prog
      .foldMap(compiler)
      .run(initProgramState)
      .value
      ._1
      .treeBuilder
      .tree

  @main
  def main: Unit =
    val prog = for
      _ <- enter
      _ <- enter
      _ <- enter
      _ <- exit(ValueTreeNode.Exchange(Map()))
      _ <- enter
      _ <- exit
      _ <- enter
      _ <- enter
    yield ()

    println:
      run(prog, Map())

trait AggregateAPI:
  type Device
  type Aggregate[_]

  def sensor[A](name: Aggregate[String]): Aggregate[A]
  // TODO: recheck
  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A]
  def exchange[A, S](init: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A]

  // TODO: should we wrap f return type in Aggregate?
  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A]

  def uid: Aggregate[Device]

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A]
    def updateSelf(f: A => A): Aggregate[A]

  extension [A](fa: Aggregate[A])
    def map[B](f: A => B): Aggregate[B]
    def flatMap[B](f: A => Aggregate[B]): Aggregate[B]

  given pureGiven[A]: Conversion[A, Aggregate[A]]

object AggregateAPI extends AggregateAPI:
  import aggregate.free.AggregateSyntax
  export AggregateSyntax.{given, *}

trait AggregateLib:
  import AggregateAPI.*
  def nbr[A](default: Aggregate[A], send: Aggregate[A]): Aggregate[A]

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A]

  def branch[A](cond: Aggregate[Boolean])(the: => Aggregate[A])(
      els: => Aggregate[A]
  ): Aggregate[A]

  def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A])

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

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] = ???

  def branch[A](cond: Aggregate[Boolean])(the: => Aggregate[A])(
      els: => Aggregate[A]
  ): Aggregate[A] = ???

  def retsend[A](a: Aggregate[A]): (Aggregate[A], Aggregate[A]) = ???

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
