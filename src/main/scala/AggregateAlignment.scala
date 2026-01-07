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

  def alignedMessages[A]: Alignment[Map[Device, A]] =
    Free.liftF(
      AlignmentGrammar
        .AlignedMessages()
        .asInstanceOf[AlignmentGrammar[Map[Device, A]]]
    )

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

    def alignedDevices: TreeState[Set[Device]] =
      for
        messages <- StateT.liftF(AggregateInput.messages)
        path <- StateT.inspect[AggregateInput, BuildingPathState, Path](_.path)
      yield messages.filter((_, t) => subtreeFromPath(t, path).isDefined).keySet

    def alignedMessages: TreeState[Map[Device, Any]] =
      for
        messages <- StateT.liftF(AggregateInput.messages)
        path <- StateT.inspect[AggregateInput, BuildingPathState, Path](_.path)
      yield messages.view
        .mapValues(t => subtreeFromPath(t, path))
        .collect({ case (d, Some(t)) => (d, t.value._2) })
        .toMap

  private def compiler: AlignmentGrammar ~> TreeState =
    new (AlignmentGrammar ~> TreeState):
      def apply[A](fa: AlignmentGrammar[A]): TreeState[A] =
        import AlignmentGrammar.*
        fa match
          case Enter(id)         => TreeState.enter(id)
          case Exit()            => TreeState.exit
          case AlignedDevices()  => TreeState.alignedDevices
          case AlignedMessages() => TreeState.alignedMessages

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
