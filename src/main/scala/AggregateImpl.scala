package aggregate

import aggregate.AggregateAPI.Device
import aggregate.AggregateAPI.Env
import aggregate.NValues.NValue
import aggregate.ValueTrees.*
import aggregate.AlignmentModule.Alignment

object AggregateImpl:
  opaque type Aggregate[+A] = Device => Alignment[A]

  def sensor[A](s: => Aggregate[A]): Aggregate[A] = s(_)

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = id =>
    for
      fNV <- f(id)
      lambda = fNV(id)
      call <- Alignment.call(
        id = lambda.toString(),
        f = () => lambda()(id)
      )
    yield call

  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] = id =>
    for
      defaultNV <- default(id)
      defaultValue = defaultNV(id)
      ret <- Alignment.alignedEnv(env =>
        val overrides =
          env.map((d, tree) =>
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
        Alignment.exchange(ret = ret(id), send = send(id))
      )
    yield ret

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] = id =>
    for
      init <- init(id)
      aNV <- a(id)
      res <- Alignment.alignedEnv(env =>
        val neighbours = env.keySet - id
        val folded =
          neighbours.foldLeft(init(id))((res, d) => f(res, aNV(d)))
        Alignment.pure(NValue(folded))
      )
    yield res

  def uid: Aggregate[Device] = id => Alignment.pure(NValue(id))

  extension [A](fa: Aggregate[A])
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] =
      fa.flatMap(a => pure(f(a)))

    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B] = id =>
      fa(id).flatMap(a => f(a)(id))

  def pure[A](a: A): Aggregate[A] = pure(NValue(a))
  def pure[A](a: NValue[A]): Aggregate[A] = _ => Alignment.pure(a)

  extension [A](a: Aggregate[A])
    def run(uid: Device)(env: Env): ValueTree[A] =
      a(uid).run(env)
