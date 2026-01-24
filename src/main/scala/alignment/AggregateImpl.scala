package alignment

import aggregate.NValues.*
import AlignmentModule.*
import aggregate.AggregateAPI.Device
import aggregate.AggregateAPI.Input

object Aggregate:
  opaque type Aggregate[+A] = Grammar[A]
  private enum Grammar[+A]:
    case Exchange[A, S](
        default: Aggregate[S],
        body: Aggregate[S] => (Aggregate[A], Aggregate[S])
    ) extends Aggregate[A]
    case NFold[A, B](init: Aggregate[A], a: Aggregate[B], f: (A, B) => A)
        extends Aggregate[A]
    case Mux(cond: Aggregate[Boolean], th: Aggregate[A], el: Aggregate[A])
    case Call(id: String, f: Aggregate[() => Aggregate[A]])
    case Sensor(name: Aggregate[String])
    case Uid extends Aggregate[Device]
    case Self(a: Aggregate[A])
    case OverrideDevice[A](fa: Aggregate[A], d: Aggregate[Device], f: A => A)
        extends Aggregate[A]
    case Pure(nvalues: NValue[A])
    case FlatMap[A, B](a: Aggregate[A], f: NValue[A] => Aggregate[B])
        extends Aggregate[B]

    // TODO: here just until we can implement call
    case Branch(cond: Aggregate[Boolean], th: Aggregate[A], el: Aggregate[A])
  import Grammar.*

  def exchange[A, S](default: Aggregate[S])(
      body: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] =
    Exchange(default, body)

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] =
    NFold(init, a, f)

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] =
    Mux(cond, th, el)

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] =
    Call(???, f)

  def sensor[A](name: Aggregate[String]): Aggregate[A] = Sensor(name)

  def uid: Aggregate[Device] = Uid

  def pure[A](a: A): Aggregate[A] = pureNV(NValue(a))

  def pureNV[A](a: NValue[A]): Aggregate[A] = Pure(a)

  extension [A](a: Aggregate[A])
    def self: Aggregate[A] = Self(a)

    def overrideDevice(d: Aggregate[Device], f: A => A): Aggregate[A] =
      OverrideDevice(a, d, f)

    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] =
      a.flatMap(a => pureNV(f(a)))

    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B] =
      FlatMap(a, f)

  // TODO: just for the moment
  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] =
    Branch(cond, th, el)

  private def fromAlignmentContext[A](
      f: Env[Device] => NValue[A]
  ): Alignment[Device, NValue[A]] =
    Alignment.alignedContext(f)

  extension [A](a: Aggregate[A])
    def runAggregate(using Env[Device], Input): AlignmentTree[NValue[A]] =
      a.toAlignment.run(summon[Env[Device]])

    def toAlignment(using input: Input): Alignment[Device, NValue[A]] =
      val uid = input.uid
      val sensors = input.sensors
      a match
        case Exchange(default, body) =>
          for
            default <- default.toAlignment
            defaultValue = default(uid)
            nbrMessages <- fromAlignmentContext(ctx =>
              val overrides =
                ctx.toMap.map((d, tree) =>
                  (d, tree.value.asInstanceOf[NValue[defaultValue.type]](d))
                )
              NValue(defaultValue, overrides)
            )
            (ret, send) = body(pureNV(nbrMessages))
            ret <- ret.toAlignment
            send <- send.toAlignment
            ret <- Alignment.xc(ret, send)
          yield ret

        case NFold(init, a, f) =>
          for
            init <- init.toAlignment
            a <- a.toAlignment
            res <- Alignment.alignedContext[Device, NValue[A]](ctx =>
              val neighbours = ctx.toMap.keySet - uid
              NValue(neighbours.foldLeft(init(uid))((res, d) => f(res, a(d))))
            )
          yield res

        case Mux(cond, th, el) =>
          for
            cond <- cond.toAlignment
            th <- th.toAlignment
            el <- el.toAlignment
          yield if cond(uid) then th else el

        case Call(id, f) =>
          for
            f <- f.toAlignment
            res <- Alignment.call(id, () => f(uid)().toAlignment)
          yield res

        case Sensor(name) =>
          for name <- name.toAlignment
          yield sensors(name(uid)).asInstanceOf[NValue[A]]

        case Uid =>
          Alignment.pure(NValue(uid))

        case Self(a) =>
          for a <- a.toAlignment
          yield NValue(a(uid))

        case OverrideDevice(a, d, f) =>
          for
            a <- a.toAlignment
            d <- d.toAlignment
          yield a.setWith(d(uid), f)

        case FlatMap(a, f) =>
          a.toAlignment.flatMap(f(_).toAlignment)

        case Pure(nvalues) =>
          Alignment.pure(nvalues)

        case Branch(cond, th, el) =>
          for
            cond <- cond.toAlignment
            condition = cond(uid)
            id = s"branch-$condition"
            res <- Alignment.call(
              id,
              () => if condition then th.toAlignment else el.toAlignment
            )
          yield res
