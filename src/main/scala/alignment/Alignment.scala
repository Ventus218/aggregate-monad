package alignment

object AlignmentModule:
  opaque type Alignment[D, +A] = Grammar[D, A]
  private enum Grammar[D, +A]:
    case Call(id: String, f: () => Alignment[D, A])
    case AlignedContext[D, A](f: (Env[D]) => Alignment[D, A])
        extends Grammar[D, A]
    case Pure[D, A](a: A) extends Grammar[D, A]
    case FlatMap[D, A, B](fa: Grammar[D, A], f: A => Grammar[D, B])
        extends Grammar[D, B]
  import Grammar.*

  object Alignment:
    def pure[D, A](a: A): Alignment[D, A] = Pure(a)

    def call[D, A](id: String, f: () => Alignment[D, A]): Alignment[D, A] =
      Call(id, f)

    def alignedContext[D, A](f: (Env[D]) => Alignment[D, A]): Alignment[D, A] =
      AlignedContext(f)

    extension [D, A](fa: Alignment[D, A])
      def map[B](f: A => B): Alignment[D, B] =
        // // Less efficient (creates more nodes)
        // fa.flatMap(a => Alignment.pure(f(a)))
        fa match
          case Call(id, fun)       => Call(id, () => fun().map(f))
          case AlignedContext(fun) => AlignedContext(env => fun(env).map(f))
          case Pure(a)             => Pure(f(a))
          case FlatMap(fa, fun)    => FlatMap(fa, a => fun(a).map(f))

      def flatMap[B](f: A => Alignment[D, B]): Alignment[D, B] =
        FlatMap(fa, f)

      def run(unsafeEnv: Env[D]): AlignmentTree[A] =
        import Env.*
        val env: Env[D] = unsafeEnv.alignWith(fa)
        fa match
          case AlignedContext(f) =>
            val alignment = f(env.asInstanceOf[Env[D]])
            alignment.run(env)
          case Pure(a) =>
            AlignmentTree.Val(a)
          case Call(id, f) =>
            val runA = f().run(env.enterChildN(0))
            AlignmentTree.Call(id, runA)
          case FlatMap(fa, f) =>
            val left = fa.run(env.enterChildN(0))
            val right = f(left.value).run(env.enterChildN(1))
            AlignmentTree.Next(left, right)

  enum AlignmentTree[+A] private:
    case Val(value: A)
    case Call(id: String, right: AlignmentTree[A])
    case Next(left: AlignmentTree[Any], right: AlignmentTree[A])
    override def toString(): String =
      def loop(t: AlignmentTree[Any], level: Int): String =
        val indent = "  " * level
        t match
          case Val(v) =>
            val head = s"${indent}NVal(v: $v)"
            head

          case Call(id, right) =>
            val head = s"${indent}Call(id: $id)"
            val r = loop(right, level + 1)
            s"$head\n$r"

          case Next(first, last) =>
            val head = s"${indent}Next"
            val f = loop(first, level + 1)
            val l = loop(last, level + 1)
            s"$head\n$f\n$l"

      loop(this, 0)

  object AlignmentTree:
    extension [A](t: AlignmentTree[A])
      def value: A =
        t match
          case AlignmentTree.Val(v)         => v
          case AlignmentTree.Call(_, right) => right.value
          case AlignmentTree.Next(_, right) => right.value
      private[AlignmentModule] def children: Seq[AlignmentTree[Any]] =
        t match
          case AlignmentTree.Val(value)        => Seq()
          case AlignmentTree.Call(id, right)   => Seq(right)
          case AlignmentTree.Next(left, right) => Seq(left, right)

  opaque type Env[D] = Map[D, AlignmentTree[Any]]
  object Env:
    def fromMap[D](m: Map[D, AlignmentTree[Any]]): Env[D] =
      m

    extension [D](env: Env[D])
      private[AlignmentModule] def alignWith[A](node: Alignment[D, A]): Env[D] =
        env.filter((_, t) =>
          (t, node) match
            case (AlignmentTree.Call(id1, _), Grammar.Call(id2, _)) =>
              id1 == id2
            case _ => true
        )

      private[AlignmentModule] def enterChildN(n: Int): Env[D] =
        env.view.mapValues(_.children(n)).toMap

      def toMap: Map[D, AlignmentTree[Any]] =
        env
