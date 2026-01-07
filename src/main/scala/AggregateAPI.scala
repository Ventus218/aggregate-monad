package aggregate

import scala.annotation.tailrec

object AggregateInput:
  import orderedtree.OrderedTree.*
  import AggregateAPI.Device
  // TODO: is this usage from AggregateAlignment ugly?
  import AggregateAlignment.AlignmentTree

  private enum AggregateInputGrammar[A]:
    case Uid() extends AggregateInputGrammar[Device]
    case Messages()
        extends AggregateInputGrammar[Map[Device, AlignmentTree[Any]]]
    case Sensor(name: String)
  import AggregateInputGrammar.*

  import cats.free.Free
  opaque type AggregateInput[A] = Free[AggregateInputGrammar, A]

  // TODO: Make private [free]
  given cats.Monad[AggregateInput] = summon[cats.Monad[AggregateInput]]

  // Re-expose flatMap and map that were hidden by using an opaque type
  extension [A](fa: AggregateInput[A])
    def flatMap[B](f: A => AggregateInput[B]): AggregateInput[B] =
      fa.flatMap(f)
    def map[B](f: A => B): AggregateInput[B] =
      fa.map(f)

  def uid: AggregateInput[Device] =
    Free.liftF(Uid())
  def messages: AggregateInput[Map[Device, AlignmentTree[Any]]] =
    Free.liftF(Messages())
  def sensor[A](name: String): AggregateInput[A] =
    Free.liftF(Sensor(name))

  import cats.~>
  import cats.data.Reader
  private case class Input(
      uid: Device,
      sensors: Map[String, Any],
      messages: Map[Device, AlignmentTree[Any]]
  )
  private type AggregateReader[A] = Reader[Input, A]

  private def compiler: AggregateInputGrammar ~> AggregateReader =
    new (AggregateInputGrammar ~> AggregateReader):
      def apply[A](fa: AggregateInputGrammar[A]): AggregateReader[A] =
        import AggregateInputGrammar.*
        fa match
          case Uid()        => Reader(_.uid)
          case Messages()   => Reader(_.messages)
          case Sensor(name) => Reader(_.sensors(name).asInstanceOf[A])

object AggregateAlignment:
  import AggregateAPI.Device
  import orderedtree.OrderedTree.*
  import AggregateInput.*

  enum AlignmentGrammar[A]:
    case Enter(id: String) extends AlignmentGrammar[Unit]
    case Exit() extends AlignmentGrammar[Unit]
    case AlignedDevices() extends AlignmentGrammar[Set[Device]]

  import cats.free.Free
  opaque type Alignment[A] = Free[AlignmentGrammar, A]

  // Re-expose flatMap and map that were hidden by using an opaque type
  extension [A](fa: Alignment[A])
    def flatMap[B](f: A => Alignment[B]): Alignment[B] =
      fa.flatMap(f)
    def map[B](f: A => B): Alignment[B] =
      fa.map(f)

  def enter(id: String): Alignment[Unit] =
    Free.liftF(AlignmentGrammar.Enter(id))

  def exit: Alignment[Unit] =
    Free.liftF(AlignmentGrammar.Exit())

  def alignedDevices: Alignment[Set[Device]] =
    Free.liftF(AlignmentGrammar.AlignedDevices())

  import cats.~>
  import cats.data.StateT

  type AlignmentTree[A] = OrderedTree[(String, A)]
  private type Path = List[String]

  private case class BuildingPathState(path: Path, levelCounter: Map[Int, Int])
  private type TreeState[A] = StateT[AggregateInput, BuildingPathState, A]
  private object TreeState:
    def enter(id: String): TreeState[Unit] =
      StateT.modify(s =>
        def helper(path: Path, level: Int = 0): (Path, Map[Int, Int]) =
          path match
            case Nil =>
              (
                List(s"$id${s.levelCounter(level)}"),
                s.levelCounter.updatedWith(level)(_.map(_ + 1))
              )
            case prev :: tail =>
              val (path, newLevelCounter) = helper(tail, level + 1)
              (prev +: path, newLevelCounter)

        val (newPath, newLevelCounter) = helper(s.path)
        BuildingPathState(newPath, newLevelCounter)
      )

    def exit: TreeState[Unit] =
      StateT.modify(s =>
        if s.path.isEmpty then
          throw IllegalStateException("Cannot exit an empty path")
        s.copy(path = s.path.dropRight(1))
      )

    private def isTreeAligned(t: AlignmentTree[Any], path: Path): Boolean =
      path match
        case Nil => true
        case id :: next =>
          t.value match
            case Some(value) =>
              id == value._1 && t.children.exists(c => isTreeAligned(c, next))
            case None => false

    def alignedDevices: TreeState[Set[Device]] =
      for
        messages <- StateT.liftF(AggregateInput.messages)
        path <- StateT.inspect[AggregateInput, BuildingPathState, Path](_.path)
      yield messages.filter((_, t) => isTreeAligned(t, path)).keySet

  private def compiler: AlignmentGrammar ~> TreeState =
    new (AlignmentGrammar ~> TreeState):
      def apply[A](fa: AlignmentGrammar[A]): TreeState[A] =
        import AlignmentGrammar.*
        fa match
          case Enter(id)        => TreeState.enter(id)
          case Exit()           => TreeState.exit
          case AlignedDevices() => TreeState.alignedDevices

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
