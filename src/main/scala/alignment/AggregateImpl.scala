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
    case Branch(
        cond: Aggregate[Boolean],
        th: () => Aggregate[A],
        el: () => Aggregate[A]
    )
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
  def branch[A](cond: Aggregate[Boolean])(th: => Aggregate[A])(
      el: => Aggregate[A]
  ): Aggregate[A] =
    Branch(cond, () => th, () => el)

  enum TypeOfValue[+A]:
    case NVal(nv: NValue[A])
    case XC[R, S](ret: NValue[R], send: NValue[S]) extends TypeOfValue[R]
    def value: NValue[A] =
      this match
        case NVal(nv)      => nv
        case XC(ret, send) => ret

  type AggregateAlignment[A] = Alignment[Device, TypeOfValue[A]]

  private def fromAlignmentContext[A](
      f: Env[Device] => AggregateAlignment[A]
  ): AggregateAlignment[A] =
    Alignment.alignedContext(f)

  extension [A](a: Aggregate[A])
    def runAggregate(using Env[Device], Input): AlignmentTree[TypeOfValue[A]] =
      a.toAlignment.run(summon[Env[Device]])

    def toAlignment(using input: Input): Alignment[Device, TypeOfValue[A]] =
      val uid = input.uid
      val sensors = input.sensors
      a match
        case Exchange(default, body) =>
          for
            default <- default.toAlignment
            defaultValue = default.value(uid)
            ret <- fromAlignmentContext(ctx =>
              val overrides =
                ctx.toMap.map((d, tree) =>
                  (
                    d,
                    tree.value
                      .asInstanceOf[TypeOfValue.XC[A, defaultValue.type]]
                      .send(d)
                  )
                )
              val nbrMessages = NValue(defaultValue, overrides)
              val (ret, send) = body(pureNV(nbrMessages))
              for
                ret <- ret.toAlignment
                send <- send.toAlignment
              yield TypeOfValue.XC(ret.value, send.value)
            )
          yield ret

        case NFold(init, a, f) =>
          for
            init <- init.toAlignment
            a <- a.toAlignment
            res <- fromAlignmentContext(ctx =>
              val neighbours = ctx.toMap.keySet - uid
              val folded =
                neighbours.foldLeft(init.value(uid))((res, d) =>
                  f(res, a.value(d))
                )
              Alignment.pure(TypeOfValue.NVal(NValue(folded)))
            )
          yield res

        case Mux(cond, th, el) =>
          for
            cond <- cond.toAlignment
            th <- th.toAlignment
            el <- el.toAlignment
          yield if cond.value(uid) then th else el

        case Call(id, f) =>
          for
            f <- f.toAlignment
            res <- Alignment.call(id, () => f.value(uid)().toAlignment)
          yield res

        case Sensor(name) =>
          for name <- name.toAlignment
          yield TypeOfValue.NVal(
            sensors(name.value(uid)).asInstanceOf[NValue[A]]
          )

        case Uid =>
          Alignment.pure(TypeOfValue.NVal(NValue(uid)))

        case Self(a) =>
          for a <- a.toAlignment
          yield TypeOfValue.NVal(NValue(a.value(uid)))

        case OverrideDevice(a, d, f) =>
          for
            a <- a.toAlignment
            d <- d.toAlignment
          yield TypeOfValue.NVal(a.value.setWith(d.value(uid), f))

        case FlatMap(a, f) =>
          a.toAlignment.flatMap(v => f(v.value).toAlignment)

        case Pure(nvalues) =>
          Alignment.pure(TypeOfValue.NVal(nvalues))

        case Branch(cond, th, el) =>
          for
            cond <- cond.toAlignment
            condition = cond.value(uid)
            id = s"branch-$condition"
            res <- Alignment.call(
              id,
              () => (if condition then th() else el()).toAlignment
            )
          yield res
