package aggregate

object AggregateAlignment:
  enum AlignmentGrammar[A]:
    case Enter(name: String) extends AlignmentGrammar[Unit]
    case Exit() extends AlignmentGrammar[Unit]

  import cats.free.Free

  type Alignment[A] = Free[AlignmentGrammar, A]

  def enter(name: String) =
    Free.liftF(AlignmentGrammar.Enter(name))
  def exit =
    Free.liftF(AlignmentGrammar.Exit())

  import cats.~>
  import cats.data.State

  import orderedtree.OrderedTree.*

  type TreeState[A] = State[LeftToRightOrderedTreeBuilder[String], A]

  object TreeState:
    import OrderedTree.*
    def enter(name: String): TreeState[Unit] =
      State.modify(s => s.enter(name))
    def exit: TreeState[Unit] =
      State.modify(s => s.exit)

  def compiler: AlignmentGrammar ~> TreeState =
    new (AlignmentGrammar ~> TreeState):
      def apply[A](fa: AlignmentGrammar[A]): TreeState[A] =
        import AlignmentGrammar.*
        fa match
          case Enter(name) => TreeState.enter(name)
          case Exit()      => TreeState.exit

  @main
  def main: Unit =
    val prog = for
      _ <- enter("sos")
      _ <- enter("sis")
      _ <- enter("sus")
      _ <- exit
      _ <- enter("yey")
      _ <- exit
      _ <- enter("yoy")
      _ <- enter("yoy")
    yield ()

    println:
      prog
        .foldMap(compiler)
        .run(LeftToRightOrderedTreeBuilder.root("root"))
        .value
        ._1
        .tree

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
