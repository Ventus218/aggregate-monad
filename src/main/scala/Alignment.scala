package aggregate

import aggregate.AggregateAPI.Env
import aggregate.NValues.*
import aggregate.ValueTrees.*

object AlignmentModule:
  opaque type Alignment[+A] = Grammar[A]
  private enum Grammar[+A]:
    case Exchange(ret: Grammar[A], send: Grammar[Any])
    case Call(id: String, f: () => Alignment[A])
    case AlignedContext(f: (Env) => Alignment[A])
    case Pure(a: NValue[A])
    case FlatMap[A, +B](fa: Grammar[A], f: NValue[A] => Grammar[B])
        extends Grammar[B]
  import Grammar.*

  object Alignment:
    def pure[A](a: NValue[A]): Alignment[A] = Pure(a)

    def call[A](id: String, f: () => Alignment[A]): Alignment[A] =
      Call(id, f)

    def exchange[A](ret: Alignment[A], send: Alignment[Any]): Alignment[A] =
      Exchange(ret, send)

    def alignedContext[A](f: (Env) => Alignment[A]): Alignment[A] =
      AlignedContext(f)

    extension [A](fa: Alignment[A])
      def map[B](f: NValue[A] => NValue[B]): Alignment[B] =
        // // Less efficient (creates more nodes)
        // fa.flatMap(a => Alignment.pure(f(a)))
        fa match
          case Call(id, fun)       => Call(id, () => fun().map(f))
          case Exchange(ret, send) => Exchange(ret.map(f), send)
          case AlignedContext(fun) => AlignedContext(env => fun(env).map(f))
          case Pure(a)             => Pure(f(a))
          case FlatMap(fa, fun)    => FlatMap(fa, a => fun(a).map(f))

      def flatMap[B](f: NValue[A] => Alignment[B]): Alignment[B] =
        FlatMap(fa, f)

      def run(unsafeEnv: Env): ValueTree[A] =
        val env: Env = unsafeEnv.alignWith(fa)
        fa match
          case AlignedContext(f) =>
            val alignment = f(env)
            alignment.run(env)
          case Pure(a) =>
            ValueTree.NVal(a)
          case Exchange(ret, send) =>
            val retTree = ret.run(env.enterChildN(0))
            val sendTree = send.run(env.enterChildN(1))
            ValueTree.Exchange(retTree, sendTree)
          case Call(id, f) =>
            val runA = f().run(env.enterChildN(0))
            ValueTree.Call(id, runA)
          case FlatMap(fa, f) =>
            val left = fa.run(env.enterChildN(0))
            val right = f(left.nv).run(env.enterChildN(1))
            ValueTree.Sequence(left, right)

    extension (env: Env)
      private def alignWith[A](a: Alignment[A]): Env =
        env.filter((_, t) =>
          (t, a) match
            case (ValueTree.Call(id1, _), Grammar.Call(id2, _)) =>
              id1 == id2
            case _ => true
        )
      private def enterChildN(n: Int): Env =
        env.view.mapValues(_.children(n)).toMap
