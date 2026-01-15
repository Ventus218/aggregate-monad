package aggregate

object ValueTrees:
  import NValues.*

  enum ValueTree[+A]:
    case NVal(nv: NValue[A], children: Seq[ValueTree[Any]])
    case XC[+R, +S](
        ret: NValue[R],
        send: NValue[S],
        children: Seq[ValueTree[Any]]
    ) extends ValueTree[R]
    case Call(id: String, nv: NValue[A], children: Seq[ValueTree[Any]])
  import ValueTree.*

  extension [A](vt: ValueTree[A])
    def nv: NValue[A] = vt match
      case NVal(nv, children)      => nv
      case XC(ret, send, children) => ret
      case Call(id, nv, children)  => nv

    def children: Seq[ValueTree[Any]] = vt match
      case NVal(nv, children)      => children
      case XC(ret, send, children) => children
      case Call(id, nv, children)  => children

  object ValueTree:
    def nval[A](nv: NValue[A], children: ValueTree[Any]*): ValueTree[A] =
      NVal(nv, children)

    def xc[R, S]( nv: NValue[R], send: NValue[S], children: ValueTree[Any]*): ValueTree[R] = 
      XC(nv, send, children)

    def call[A]( id: String, nv: NValue[A], children: ValueTree[Any]*): ValueTree[A] =
      Call(id, nv, children)
