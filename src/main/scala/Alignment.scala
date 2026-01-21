package alignment

object AlignmentModule:

  opaque type Alignment[D, +A] = Grammar[D, A]
  private enum Grammar[D, +A]:
    case Enter[D, A]() extends Grammar[D, A]
    case Exit[D, A]() extends Grammar[D, A]
    case AlignedContext[D, A]() extends Grammar[D, Map[D, A]]

    case Pure[D, A](a: A) extends Grammar[D, A]
    case FlatMap[D, A, B](fa: Grammar[D, A], f: A => Grammar[D, B])
        extends Grammar[D, B]

  import Grammar.*

  object Alignment:
    def pure[D, A](a: A): Alignment[D, A] = Pure(a)

    def enter[D, A]: Alignment[D, A] = Enter()
    def exit[D, A]: Alignment[D, A] = Exit()
    def alignedContext[D, A]: Alignment[D, Map[D, A]] = AlignedContext()

  extension [D, A](fa: Alignment[D, A])
    def map[B](f: A => B): Alignment[D, B] =
      fa.flatMap(a => Alignment.pure(f(a)))
    def flatMap[B](f: A => Alignment[D, B]): Alignment[D, B] =
      FlatMap(fa, f)

  enum AlignmentTree[+A]:
    case Val(value: A)
    case Call(id: String, value: A, children: Seq[AlignmentTree[Any]])
    case Next(left: AlignmentTree[Any], right: AlignmentTree[A])

  extension [A](t: AlignmentTree[A])
    def value: A =
      t match
        case AlignmentTree.Val(v)         => v
        case AlignmentTree.Call(_, v, _)  => v
        case AlignmentTree.Next(_, right) => right.value

  extension [D, A](fa: Alignment[D, A])
    def run: AlignmentTree[A] =
      fa match
        case Enter()          => ???
        case Exit()           => ???
        case AlignedContext() => ???
        case Pure(a) =>
          AlignmentTree.Val(a)
        case FlatMap(fa, f) =>
          val left = fa.run
          val right = f(left.value).run
          AlignmentTree.Next(left, right)

object Test:
  import aggregate.NValues.*
  import AlignmentModule.*
  import cats.~>
  import cats.free.Free
  import aggregate.AggregateAPI.Device

  given cats.Monad[NValue] = ???
  given cats.Monad[AggregateAlignment] = ???

  case class Input(uid: Device, sensors: Map[String, NValue[Any]])
  private enum AggregateAlgebra[A]:
    case Exchange[A, S](
        default: Aggregate[S],
        body: Aggregate[S] => (Aggregate[A], Aggregate[S])
    ) extends AggregateAlgebra[A]
    case NFold[A, B](init: Aggregate[A], a: Aggregate[B], f: (A, B) => A)
        extends AggregateAlgebra[A]
    case Mux(cond: Aggregate[Boolean], th: Aggregate[A], el: Aggregate[A])
    case Call(id: String, f: Aggregate[() => Aggregate[A]])
    case Sensor(name: Aggregate[String])
    case Uid extends AggregateAlgebra[Device]
    case Self(a: Aggregate[A])
    case OverrideDevice[A](fa: Aggregate[A], d: Aggregate[Device], f: A => A)
        extends AggregateAlgebra[A]

  opaque type Aggregate[A] = Free[AggregateAlgebra, NValue[A]]

  def countAlignedNeighbours: Aggregate[Int] = ???
  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] = ???
  def cond: Aggregate[Boolean] = ???

  type AggregateAlignment[A] = Alignment[Device, A]

  private def compiler: AggregateAlgebra ~> AggregateAlignment =
    new (AggregateAlgebra ~> AggregateAlignment):
      def apply[A](fa: AggregateAlgebra[A]): AggregateAlignment[A] =
        val uid: Device = ???
        fa match
          case AggregateAlgebra.Exchange(default, body) => ???
          case AggregateAlgebra.NFold(init, a, f)       => ???
          case AggregateAlgebra.Mux(cond, th, el)       => ???
          case AggregateAlgebra.Call(id, f)             => ???
          case AggregateAlgebra.Sensor(name)            => ???
          case AggregateAlgebra.Uid =>
            val uid: Device = ???
            Alignment.pure(uid)
          case AggregateAlgebra.Self(a) =>
            for
              _ <- Alignment.enter
              a <- a.foldMap(compiler)
            yield a(uid)
          case AggregateAlgebra.OverrideDevice(fa, d, f) => ???

  extension [A](ag: Aggregate[A])
    private def myrun: Alignment[Device, NValue[A]] =
      ag.foldMap(compiler)

  given [A]: Conversion[NValue[A], Aggregate[A]] = ???
  import scala.language.implicitConversions

  def program: Aggregate[Int] =
    for
      count <- countAlignedNeighbours
      res <- branch(cond)(count)(count)
    yield res
