package aggregate.free

import scala.language.implicitConversions
import AggregateSyntax.*
import scala.annotation.tailrec

object AggregateSemantic:

  import cats.data.State
  import cats.data.ReaderT
  import cats.data.Reader
  type ValueTreeState[A] = State[Output[Any], A]
  // opaque type Round[A] = ReaderT[ValueTreeState, Env, A]
  opaque type Round[A] = ReaderT[OrderedTree, Env, A]

  object Round:
    def env: Round[Env] = ReaderT.ask
    def uid: Round[Device] = env.map(_.uid)
    def sensors: Round[Map[String, Any]] = env.map(_.sensors)
    def appendPath(p: Path): Round[Unit] =
      ReaderT.liftF(State.modify(_.appendPath(p)))

  opaque type Env = EnvImpl
  case class EnvImpl(
      valueTrees: Map[Device, ValueTree[Any]],
      uid: Device,
      sensors: Map[String, Any]
  )
  extension (e: Env)
    def uid: Device = e.uid
    def sensors: Map[String, Any] = e.sensors
    def alignedNeighbours(path: Path): Set[Device] = ???
    def alignedMessaged(path: Path): Map[Device, Any] = ???

  case class NbrMap[A](default: A, values: Map[Device, A]):
    def apply(d: Device): A = values.get(d).getOrElse(default)

  case class Output[A](valueTree: ValueTree[A], path: Path):
    def appendPath(p: Path): Output[A] =
      Output(valueTree, path.append(p))
  object Output:
    def empty[A]: Output[A] = Output(ValueTree.Node(Seq()), Path.Empty)

  enum Path:
    case Empty
    case Segment(name: String, next: Path)
    def append(p: Path): Path =
      this match
        case Empty               => p
        case Segment(name, next) => Segment(name, next.append(p))

  enum ValueTree[+A]:
    case Empty
    case Exchange(values: NbrMap[A], next: ValueTree[A])
    case Call(name: String, child: ValueTree[A], next: ValueTree[A])

    def values: Option[NbrMap[A]] =
      this match
        case Empty                   => None
        case Exchange(values, next)  => Some(values)
        case Call(name, child, next) => child.values.orElse(next.values)
    def appendAtPath(p: Path, vt: ValueTree[A]): ValueTree[A] =
      p match
        case Path.Empty =>
          this match
            case Empty => Empty
            case Exchange(values, next) =>
              Exchange(values, next.appendAtPath(Path.Empty, vt))
            case Call(name, child, next) =>
              Call(name, child, next.appendAtPath(Path.Empty, vt))
        case Path.Segment(name, next) =>
          this match
            case Empty => throw IllegalStateException()
            case Exchange(values, next) =>
              require(name == "exchange")
              ???
            case Call(name, child, next) => ???

  import cats.free.Free
  import cats.Id
  import cats.~>

  enum OrderedTreeGrammar[A]:
    case Empty()
    case Node(value: A, child: OrderedTree[A], sibling: OrderedTree[A])
  type OrderedTree[A] = Free[OrderedTreeGrammar, A]
  object OrderedTree:
    import OrderedTreeGrammar.*
    def apply[A](value: A): OrderedTree[A] = Free.pure(value)
    def empty = Free.liftF(Empty())
    def node[A](
        value: A,
        child: OrderedTree[A],
        sibling: OrderedTree[A]
    ): OrderedTree[A] = Free.liftF(Node(value, child, sibling))

  type Tree[_]

  def treeCompiler: OrderedTreeGrammar ~> ([X] =>> Id[Tree[X]]) =
    new (OrderedTreeGrammar ~> ([X] =>> Id[Tree[X]])):
      def apply[A](fa: OrderedTreeGrammar[A]): Id[Tree[A]] =
        fa match
          case OrderedTreeGrammar.Node(value, child, sibling) =>
            val ch = child.foldMap(treeCompiler)
            ???

  def compiler: AggregateGrammar ~> Round =
    new (AggregateGrammar ~> Round):
      def apply[A](fa: AggregateGrammar[A]): Round[A] =
        import AggregateGrammar.*
        fa match
          case Uid() => Round.uid
          case Sensor(name) =>
            for
              name <- name.foldMap(compiler)
              sensors <- Round.sensors
            yield sensors(name).asInstanceOf[A]
          case Self(a) =>
            for
              env <- Round.env
              (output, _) = a.foldMap(compiler).run(env).run(Output.empty).value
              _ <- ReaderT.liftF(State.set(output.cop))
              _ <- Round.appendPath
            yield output.valueTree.values.get(env.uid) // TODO: assertion here
          case Exchange(init, body) => ???
          case NFold(init, a, f)    => ???
          case Call(f)              => ???
          case UpdateSelf(fa, f)    => ???
          case Map(fa, f)           => ???
          case FlatMap(fa, f)       => ???
