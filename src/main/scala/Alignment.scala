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

      def run(env: Env): ValueTree[A] =
        fa match
          case AlignedContext(f) => f(env).run(env)
          case Pure(a)           => ValueTree.NVal(a)
          case Exchange(ret, send) =>
            val retTree = ret.run(env.enter[ValueTree.Exchange[?, ?]](_.ret))
            val sendTree = send.run(env.enter[ValueTree.Exchange[?, ?]](_.send))
            ValueTree.Exchange(retTree, sendTree)
          case Call(id, f) =>
            val alignedEnv = env.collect({
              case (d, t @ ValueTree.Call(id1, _)) if id == id1 => (d, t)
            })
            val runA = f().run(alignedEnv.enter[ValueTree.Call[?]](_.f))
            ValueTree.Call(id, runA)
          case FlatMap(fa, f) =>
            val before = fa.run(env.enter[ValueTree.Sequence[?]](_.before))
            val after =
              f(before.nv).run(env.enter[ValueTree.Sequence[?]](_.after))
            ValueTree.Sequence(before, after)

    import scala.reflect.ClassTag
    extension (env: Env)
      private def enter[T <: ValueTree[?]: ClassTag](
          f: T => ValueTree[Any]
      ): Env =
        env.collect({ case (d, t: T) => (d, f(t)) })
