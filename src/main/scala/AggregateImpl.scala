package aggregate.nonfree

import aggregate.AggregateAPI.Device
import aggregate.AggregateAPI.Input
import aggregate.NValues.NValue
import aggregate.ValueTrees.*

object AggregateImpl:
  opaque type Aggregate[+A] = Grammar[A]
  private enum Grammar[+A]:
    case Exchange[A, S](
        default: Aggregate[S],
        body: Aggregate[S] => (Aggregate[A], Aggregate[S])
    ) extends Aggregate[A]
    case NFold[A, B](init: Aggregate[A], a: Aggregate[B], f: (A, B) => A)
        extends Aggregate[A]
    case Call(f: Aggregate[() => Aggregate[A]])
    case Sensor(name: Aggregate[String])
    case Uid extends Aggregate[Device]
    case Self(a: Aggregate[A])
    case OverrideDevice[A](fa: Aggregate[A], d: Aggregate[Device], f: A => A)
        extends Aggregate[A]
    case Pure(nvalues: NValue[A])
    case FlatMap[A, B](a: Aggregate[A], f: NValue[A] => Aggregate[B])
        extends Aggregate[B]

  import Grammar.*

  def sensor[A](name: Aggregate[String]): Aggregate[A] =
    Sensor(name)

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] =
    Call(f)

  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] = Exchange(default, f)

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] = NFold(init, a, f)

  def uid: Aggregate[Device] = Uid

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A] = Self(fa)
    def update(d: Aggregate[Device], f: A => A): Aggregate[A] =
      OverrideDevice(fa, d, f)

  extension [A](fa: Aggregate[A])
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] =
      fa.flatMap(a => Pure(f(a)))

    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B] = FlatMap(fa, f)

  def pure[A](a: A): Aggregate[A] = Pure(NValue(a))
  def pure[A](a: NValue[A]): Aggregate[A] = Pure(a)

  extension [A](nv: NValue[A])
    private def selfValue(using input: Input): A =
      nv(input.uid)

  import aggregate.AggregateAPI.Env
  import aggregate.nonfree.AlignmentModule.*

  extension [A](a: Aggregate[A])
    def run(using Env, Input): ValueTree[A] =
      a.toAlignment.run(summon[Env])

    private def toAlignment(using input: Input): Alignment[A] =
      val uid = input.uid
      val sensors = input.sensors
      a match
        case Pure(nvalues) => Alignment.pure(nvalues)

        case Uid => Alignment.pure(NValue(input.uid))

        case Self(a) =>
          for a <- a.toAlignment
          yield NValue(a.selfValue)

        case OverrideDevice(a, d, f) =>
          for
            a <- a.toAlignment
            d <- d.toAlignment
          yield a.setWith(d.selfValue, f)

        case Sensor(name) =>
          for
            name <- name.toAlignment
            sensorNValue = input.sensors(name.selfValue).asInstanceOf[NValue[A]]
          yield sensorNValue

        case FlatMap(a, f) =>
          a.toAlignment.flatMap(a => f(a).toAlignment)

        case NFold(init, a, f) =>
          for
            init <- init.toAlignment
            a <- a.toAlignment
            res <- Alignment.alignedContext(ctx =>
              val neighbours = ctx.toMap.keySet - uid
              val folded =
                neighbours.foldLeft(init(uid))((res, d) => f(res, a(d)))
              Alignment.pure(NValue(folded))
            )
          yield res

        case Exchange(default, body) =>
          for
            default <- default.toAlignment
            defaultValue = default(uid)
            ret <- Alignment.alignedContext(ctx =>
              val overrides =
                ctx.toMap.map((d, tree) =>
                  (
                    d,
                    tree
                      .asInstanceOf[ValueTree.Exchange[A, defaultValue.type]]
                      .send
                      .nv(d)
                  )
                )
              val nbrMessages = NValue(defaultValue, overrides)
              val (ret, send) = body(pure(nbrMessages))
              Alignment.xc(ret.toAlignment, send.toAlignment)
            )
          yield ret

        case Call(f) =>
          for
            f <- f.toAlignment
            call <- Alignment.call(() => f.selfValue().toAlignment)
          yield call
