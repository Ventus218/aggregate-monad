package aggregate

object NValues:
  import aggregate.AggregateAPI.Device
  case class NValue[+A](default: A, values: Map[Device, A] = Map()):
    def apply(d: Device): A = values.get(d).getOrElse(default)

  opaque type Path = List[Choice]
  private enum Choice:
    case ChildNode(nChild: Int)
    case ChildCall(nChild: Int, id: String)

  object Path:
    def empty: Path = List()

  extension (p: Path) def exit: Path = p.dropRight(1)

  opaque type ValueTree[+A] = ValueTreeImpl[A]
  private enum ValueTreeImpl[+A]:
    case Node(nv: NValue[A], children: Seq[ValueTree[A]])
    case Call(id: String, nv: NValue[A], children: Seq[ValueTree[A]])

  object ValueTree:
    def node[A](
        nv: NValue[A],
        children: Seq[ValueTree[A]] = Seq()
    ): ValueTree[A] =
      ValueTreeImpl.Node(nv, children)

    def call[A](
        id: String,
        nv: NValue[A],
        children: Seq[ValueTree[A]] = Seq()
    ): ValueTree[A] =
      ValueTreeImpl.Call(id, nv, children)

    import Choice.*
    extension [A](vt: ValueTree[A])
      def nvalue: NValue[A] = vt match
        case ValueTreeImpl.Node(nv, _)    => nv
        case ValueTreeImpl.Call(_, nv, _) => nv

      def children: Seq[ValueTree[A]] = vt match
        case ValueTreeImpl.Node(_, children)    => children
        case ValueTreeImpl.Call(_, _, children) => children

      def setNValue(nv: NValue[A]): ValueTree[A] =
        vt match
          case vt: ValueTreeImpl.Node[A] => vt.copy(nv = nv)
          case vt: ValueTreeImpl.Call[A] => vt.copy(nv = nv)

      private def appendChild(ch: ValueTree[A]): ValueTree[A] =
        vt match
          case ValueTreeImpl.Node(nv, children) =>
            ValueTreeImpl.Node(nv, children :+ ch)
          case ValueTreeImpl.Call(id, nv, children) =>
            ValueTreeImpl.Call(id, nv, children :+ ch)

      /** Returns None if the tree doesn't contain the path */
      def subtreeAt(p: Path): Option[ValueTree[A]] =
        val children = vt.children
        p match
          case Nil => Some(vt)
          case ChildCall(nChild, id) :: next =>
            children.lift(nChild) match
              case Some(ch @ ValueTreeImpl.Call(`id`, _, _)) =>
                ch.subtreeAt(next)
              case _ => None
          case ChildNode(nChild) :: next =>
            children.lift(nChild) match
              case Some(ch @ ValueTreeImpl.Node(_, _)) => ch.subtreeAt(next)
              case _                                   => None

      /** Returns None if the tree doesn't contain the path */
      def setNValueAt(nv: NValue[A], p: Path): Option[ValueTree[A]] =
        val children = vt.children
        p match
          case Nil => Some(vt.setNValue(nv))
          case ChildCall(nChild, id) :: next =>
            children.lift(nChild) match
              case Some(ch @ ValueTreeImpl.Call(`id`, _, _)) =>
                ch.setNValueAt(nv, next)
              case _ => None
          case ChildNode(nChild) :: next =>
            children.lift(nChild) match
              case Some(ch @ ValueTreeImpl.Node(_, _)) =>
                ch.setNValueAt(nv, next)
              case _ => None

      def appendNodeAt(nv: NValue[A], p: Path): Option[(ValueTree[A], Path)] =
        vt.appendAtPath(ValueTree.node(nv), p)
          .map(t => (t, p :+ ChildNode(t.children.length - 1)))

      def appendCallAt(
          id: String,
          nv: NValue[A],
          p: Path
      ): Option[(ValueTree[A], Path)] =
        vt.appendAtPath(ValueTree.call(id, nv), p)
          .map(t => (t, p :+ ChildCall(t.children.length - 1, id)))

      def appendAtPath(
          appending: ValueTree[A],
          p: Path
      ): Option[ValueTree[A]] =
        val children = vt.children
        p match
          case Nil => Some(vt.appendChild(appending))
          case ChildCall(nChild, id) :: next =>
            children.lift(nChild) match
              case Some(ch @ ValueTreeImpl.Call(`id`, _, _)) =>
                ch.subtreeAt(next)
              case _ => None
          case ChildNode(nChild) :: next =>
            children.lift(nChild) match
              case Some(ch @ ValueTreeImpl.Node(_, _)) =>
                ch.appendAtPath(appending, next)
              case _ => None
