package aggregate.free

object AggregateAlignment:
  import aggregate.AggregateAPI.Device
  import orderedtree.OrderedTree.*
  import AggregateInput.*

  enum AlignmentGrammar[A]:
    case Enter(id: String) extends AlignmentGrammar[Unit]
    case Exit() extends AlignmentGrammar[Unit]
    case AlignedDevices() extends AlignmentGrammar[Set[Device]]
    case AlignedMessages() extends AlignmentGrammar[Map[Device, Any]]
    case Uid() extends AlignmentGrammar[Device]
    case Sensor(name: String)

  import cats.free.Free
  opaque type Alignment[A] = Free[AlignmentGrammar, A]

  private[free] given cats.Monad[Alignment] =
    summon[cats.Monad[[A] =>> Free[AlignmentGrammar, A]]]

  // Re-expose flatMap and map that were hidden by using an opaque type
  extension [A](fa: Alignment[A])
    def flatMap[B](f: A => Alignment[B]): Alignment[B] =
      fa.flatMap(f)
    def map[B](f: A => B): Alignment[B] =
      fa.map(f)

  object Alignment:
    def enter(id: String): Alignment[Unit] =
      Free.liftF(AlignmentGrammar.Enter(id))

    def exit: Alignment[Unit] =
      Free.liftF(AlignmentGrammar.Exit())

    def alignedDevices: Alignment[Set[Device]] =
      Free.liftF(AlignmentGrammar.AlignedDevices())

    def alignedMessages[A]: Alignment[Map[Device, A]] =
      Free.liftF(
        AlignmentGrammar
          .AlignedMessages()
          .asInstanceOf[AlignmentGrammar[Map[Device, A]]]
      )

    def uid: Alignment[Device] =
      Free.liftF(AlignmentGrammar.Uid())

    def sensor[A](name: String): Alignment[A] =
      Free.liftF(AlignmentGrammar.Sensor(name))

  import cats.~>
  import cats.data.StateT

  type AlignmentTree[A] = OrderedTree[(String, A)]
  private type Path = List[String]

  private case class BuildingPathState(path: Path, pathCounter: Map[Path, Int])
  private type AlignmentState[A] = StateT[AggregateInput, BuildingPathState, A]
  private object AlignmentState:
    def enter(id: String): AlignmentState[Unit] =
      StateT.modify(s =>
        val pathCounter = s.pathCounter.get(s.path).getOrElse(0)
        val newPath = s.path :+ s"$id-$pathCounter"
        val newPathCounter = s.pathCounter.updated(s.path, pathCounter + 1)
        BuildingPathState(newPath, newPathCounter)
      )

    def exit: AlignmentState[Unit] =
      StateT.modify(s =>
        if s.path.isEmpty then
          throw IllegalStateException("Cannot exit an empty path")
        s.copy(path = s.path.dropRight(1))
      )

    private def subtreeFromPath(
        t: AlignmentTree[Any],
        path: Path
    ): Option[AlignmentTree[Any]] =
      path match
        case Nil                         => None
        case id :: _ if t.value._1 != id => None
        case id :: next =>
          LazyList
            .from(t.children)
            .map(subtreeFromPath(_, next))
            .collect({ case Some(value) => value })
            .headOption

    def alignedDevices: AlignmentState[Set[Device]] =
      for
        messages <- StateT.liftF(AggregateInput.messages)
        path <- StateT.inspect[AggregateInput, BuildingPathState, Path](_.path)
      yield messages.filter((_, t) => subtreeFromPath(t, path).isDefined).keySet

    def alignedMessages: AlignmentState[Map[Device, Any]] =
      for
        messages <- StateT.liftF(AggregateInput.messages)
        path <- StateT.inspect[AggregateInput, BuildingPathState, Path](_.path)
      yield messages.view
        .mapValues(t => subtreeFromPath(t, path))
        .collect({ case (d, Some(t)) => (d, t.value._2) })
        .toMap

  private def compiler: AlignmentGrammar ~> AlignmentState =
    new (AlignmentGrammar ~> AlignmentState):
      def apply[A](fa: AlignmentGrammar[A]): AlignmentState[A] =
        import AlignmentGrammar.*
        fa match
          case Enter(id)         => AlignmentState.enter(id)
          case Exit()            => AlignmentState.exit
          case AlignedDevices()  => AlignmentState.alignedDevices
          case AlignedMessages() => AlignmentState.alignedMessages
          case Uid()             => StateT.liftF(AggregateInput.uid)
          case Sensor(name)      => StateT.liftF(AggregateInput.sensor(name))

  extension [A](prog: Alignment[A])
    def run(
        uid: Device,
        sensors: Map[String, Any],
        messages: Map[Device, AlignmentTree[Any]]
    ) =
      prog
        .foldMap(compiler)
        .run(BuildingPathState(List("root"), Map()))
        .map(_._2)
        .run(uid, sensors, messages)
