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

  enum OrderedTree[+A]:
    case Empty
    case Node(value: A, child: OrderedTree[A], sibling: OrderedTree[A])
    override def toString(): String =
      this match
        case Empty => "_"
        case Node(value, child, sibling) =>
          s"Node($value, ${child.toString()}, ${sibling.toString()})"
  object OrderedTree:
    extension [A](t: OrderedTree[A])
      /** Expects a vaild path for appending (not inserting) */
      def appendSiblingAtPath(s: OrderedTree[A], path: Path): OrderedTree[A] =
        t match
          case Empty =>
            require(path.isEmpty, "path doesn't match the tree")
            s
          case Node(value, child, sibling) =>
            path match
              case Nil =>
                Node(value, child, sibling.appendSiblingAtPath(s, Nil))
              case Choice.Child :: t =>
                Node(value, child.appendSiblingAtPath(s, t), sibling)
              case Choice.Sibling :: t =>
                Node(value, child, sibling.appendSiblingAtPath(s, t))

      /** Expects a vaild path for appending (not inserting) */
      def appendChildAtPath(c: OrderedTree[A], path: Path): OrderedTree[A] =
        t match
          case Empty =>
            throw IllegalArgumentException(
              "Can't append a child to an Empty tree"
            )
          case Node(value, child, sibling) =>
            path match
              case Nil =>
                Node(value, child.appendSiblingAtPath(c, Nil), sibling)
              case Choice.Child :: t =>
                Node(value, child.appendChildAtPath(c, t), sibling)
              case Choice.Sibling :: t =>
                Node(value, child, sibling.appendChildAtPath(c, t))

      def subtreeAtPath(path: Path): OrderedTree[A] =
        t match
          case Empty =>
            require(path.isEmpty, "path doesn't match the tree")
            Empty
          case Node(value, child, sibling) =>
            path match
              case Nil                 => t
              case Choice.Child :: t   => child.subtreeAtPath(t)
              case Choice.Sibling :: t => sibling.subtreeAtPath(t)

      def children: Seq[OrderedTree[A]] =
        t match
          case Empty             => Seq()
          case Node(_, Empty, _) => Seq()
          case Node(_, child, _) => Seq(child) ++ child.siblings

      def siblings: Seq[OrderedTree[A]] =
        t match
          case Empty               => Seq()
          case Node(_, _, Empty)   => Seq()
          case Node(_, _, sibling) => Seq(sibling) ++ sibling.siblings

  enum Choice:
    case Child
    case Sibling

  type Path = List[Choice]

  case class AlignmentState(tree: OrderedTree[Any], path: Path)
  type TreeState[A] = State[AlignmentState, A]

  object TreeState:
    import OrderedTree.*
    import Choice.*
    def enter(name: String): TreeState[Unit] =
      State.modify(s =>
        val newTree = s.tree.appendChildAtPath(Node(name, Empty, Empty), s.path)
        val newPath = (s.path :+ Child) ++ newTree
          .subtreeAtPath(s.path)
          .children
          .drop(1)
          .foldLeft(List.empty[Choice])((p, _) => p :+ Sibling)
        AlignmentState(newTree, newPath)
      )
    def exit: TreeState[Unit] =
      State.modify(s =>
        val newPath = s.path.reverse.dropWhile(_ == Sibling).drop(1).reverse
        s.copy(path = newPath)
      )

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
        .run(
          AlignmentState(
            OrderedTree.Node("root", OrderedTree.Empty, OrderedTree.Empty),
            Nil
          )
        )
        .value
        ._1

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
