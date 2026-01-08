package aggregate.free

object AggregateInput:
  import orderedtree.OrderedTree.*
  import aggregate.AggregateAPI.Device
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

  private[free] given cats.Monad[AggregateInput] =
    summon[cats.Monad[[A] =>> Free[AggregateInputGrammar, A]]]

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

  extension [A](prog: AggregateInput[A])
    def run(
        uid: Device,
        sensors: Map[String, Any],
        messages: Map[Device, AlignmentTree[Any]]
    ) =
      prog.foldMap(compiler).run(Input(uid, sensors, messages))
