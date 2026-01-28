package aggregate

import aggregate.AggregateAPI.Device
import aggregate.AggregateAPI.Input
import aggregate.AggregateAPI.Env
import aggregate.NValues.NValue
import aggregate.ValueTrees.*
import aggregate.AlignmentModule.Alignment

object AggregateImpl:
  opaque type Aggregate[+A] = Device => Alignment[A]

  def sensor[A](s: => Aggregate[A]): Aggregate[A] = s(_)

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = id =>
    for
      f <- f(id)
      lambda = f(id)
      call <- Alignment.call(
        lambda.toString(),
        () => lambda()(id)
      )
    yield call

  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] = id =>
    for
      default <- default(id)
      defaultValue = default(id)
      ret <- Alignment.alignedContext(ctx =>
        val overrides =
          ctx.toMap.map((d, tree) =>
            (
              d,
              tree
                .asInstanceOf[ValueTree.Exchange[A, defaultValue.type]]
                .send
                .nv(id)
            )
          )
        val nbrMessages = NValue(defaultValue, overrides)
        val (ret, send) = f(pure(nbrMessages))
        Alignment.xc(ret(id), send(id))
      )
    yield ret

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] = id =>
    for
      init <- init(id)
      a <- a(id)
      res <- Alignment.alignedContext(ctx =>
        val neighbours = ctx.toMap.keySet - id
        val folded =
          neighbours.foldLeft(init(id))((res, d) => f(res, a(d)))
        Alignment.pure(NValue(folded))
      )
    yield res

  def uid: Aggregate[Device] = id => Alignment.pure(NValue(id))

  extension [A](fa: Aggregate[A])
    def update(d: Aggregate[Device], f: A => A): Aggregate[A] = id =>
      for
        fa <- fa(id)
        d <- d(id)
      yield fa.setWith(d(id), f)

  extension [A](fa: Aggregate[A])
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] =
      fa.flatMap(a => pure(f(a)))

    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B] = id =>
      fa(id).flatMap(a => f(a)(id))

  def pure[A](a: A): Aggregate[A] = pure(NValue(a))
  def pure[A](a: NValue[A]): Aggregate[A] = _ => Alignment.pure(a)

  extension [A](a: Aggregate[A])
    def run(using env: Env, input: Input): ValueTree[A] =
      a(input.uid).run(env)
