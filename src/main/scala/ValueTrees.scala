package aggregate

object ValueTrees:
  import NValues.*

  enum ValueTree[+A]:
    case Sequence(first: ValueTree[Any], last: ValueTree[A])
    case NVal(nv: NValue[A], children: Seq[ValueTree[Any]])
    case XC[+R, +S](
        ret: NValue[R],
        send: NValue[S],
        children: Seq[ValueTree[Any]]
    ) extends ValueTree[R]
    case Call(id: String, nv: NValue[A], children: Seq[ValueTree[Any]])

    override def toString(): String =
      def loop(t: ValueTree[Any], level: Int): String =
        val indent = "  " * level
        t match
          case NVal(nv, ch) =>
            val head = s"${indent}NVal(nv: $nv):"
            val children =
              if ch.isEmpty then ""
              else ch.map(c => loop(c, level + 1)).mkString("\n")
            if children.isEmpty then head else s"$head\n$children"

          case XC(ret, send, ch) =>
            val head = s"${indent}XC(nv: $ret, send: $send):"
            val children =
              if ch.isEmpty then ""
              else ch.map(c => loop(c, level + 1)).mkString("\n")
            if children.isEmpty then head else s"$head\n$children"

          case Call(id, nv, ch) =>
            val head = s"${indent}Call(id: $id):"
            val children =
              if ch.isEmpty then ""
              else ch.map(c => loop(c, level + 1)).mkString("\n")
            if children.isEmpty then head else s"$head\n$children"

          case Sequence(first, last) =>
            val head = s"${indent}Sequence:"
            val f = loop(first, level + 1)
            val l = loop(last, level + 1)
            s"$head\n$f\n$l"

      loop(this, 0)

  import ValueTree.*

  extension [A](vt: ValueTree[A])
    def nv: NValue[A] = vt match
      case NVal(nv, children)       => nv
      case XC(ret, send, children)  => ret
      case Call(id, nv, children)   => nv
      case Sequence(children, last) => last.nv

    def children: Seq[ValueTree[Any]] = vt match
      case NVal(nv, children)      => children
      case XC(ret, send, children) => children
      case Call(id, nv, children)  => children
      case Sequence(first, last)   => Seq(first, last)

  object ValueTree:
    def nval[A](nv: NValue[A], children: ValueTree[Any]*): ValueTree[A] =
      NVal(nv, children)

    def xc[R, S](
        nv: NValue[R],
        send: NValue[S],
        children: ValueTree[Any]*
    ): ValueTree[R] =
      XC(nv, send, children)

    def call[A](
        id: String,
        nv: NValue[A],
        children: ValueTree[Any]*
    ): ValueTree[A] =
      Call(id, nv, children)

    def seq[A](
        first: ValueTree[Any],
        last: ValueTree[A]
    ): ValueTree[A] =
      Sequence(first, last)
