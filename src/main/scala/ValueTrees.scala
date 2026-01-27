package aggregate

object ValueTrees:
  import NValues.*

  enum ValueTree[+A]:
    case Sequence(before: ValueTree[Any], after: ValueTree[A])
    case NVal(nv: NValue[A])
    case Exchange[+R, +S](
        ret: ValueTree[R],
        send: ValueTree[S]
    ) extends ValueTree[R]
    case Call(id: String, f: ValueTree[A])

    override def toString(): String =
      def indented(t: ValueTree[Any], level: Int): String =
        val str = t match
          case NVal(nv) =>
            s"NVal(nv: $nv):"
          case Exchange(ret, send) =>
            val children = t.children.map(indented(_, level + 1)).mkString("\n")
            s"XC:\n$children"
          case Call(id, f) =>
            val funStr = indented(f, level + 1)
            s"Call(id: $id):\n$funStr"
          case Sequence(before, after) =>
            val children = t.children.map(indented(_, level + 1)).mkString("\n")
            s"Sequence:\n$children"

        val indent = "  " * level
        s"$indent$str"

      indented(this, 0)

  import ValueTree.*

  extension [A](vt: ValueTree[A])
    def nv: NValue[A] = vt match
      case NVal(nv)                => nv
      case Exchange(ret, send)     => ret.nv
      case Call(id, f)             => f.nv
      case Sequence(before, after) => after.nv

    def children: Seq[ValueTree[Any]] = vt match
      case NVal(nv)                => Seq()
      case Exchange(ret, send)     => Seq(ret, send)
      case Call(id, f)             => Seq(f)
      case Sequence(before, after) => Seq(before, after)
