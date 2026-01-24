package alignment

object AlignmentModule:

  opaque type Alignment[D, +A] = Grammar[D, A]
  private enum Grammar[D, +A]:
    case XC(ret: A, send: Any)
    case Call(id: String, f: () => Alignment[D, A])
    case AlignedContext[D, A](f: (Env[D]) => A) extends Grammar[D, A]
    case Pure[D, A](a: A) extends Grammar[D, A]
    case FlatMap[D, A, B](fa: Grammar[D, A], f: A => Grammar[D, B])
        extends Grammar[D, B]

  import Grammar.*

  object Alignment:
    def pure[D, A](a: A): Alignment[D, A] = Pure(a)
    def call[D, A](id: String, f: () => Alignment[D, A]): Alignment[D, A] =
      Call(id, f)
    def xc[D, A](ret: A, send: Any): Alignment[D, A] =
      XC(ret, send)
    def alignedContext[D, A](
        f: (Env[D]) => A
    ): Alignment[D, A] =
      AlignedContext(f)

  extension [D, A](fa: Alignment[D, A])
    def map[B](f: A => B): Alignment[D, B] =
      fa.flatMap(a => Alignment.pure(f(a)))
    def flatMap[B](f: A => Alignment[D, B]): Alignment[D, B] =
      FlatMap(fa, f)

  enum AlignmentTree[+A]:
    case Val(value: A)
    case XC(ret: A, send: Any)
    case Call(id: String, right: AlignmentTree[A])
    case Next(left: AlignmentTree[Any], right: AlignmentTree[A])
    override def toString(): String =
      def loop(t: AlignmentTree[Any], level: Int): String =
        val indent = "  " * level
        t match
          case Val(v) =>
            val head = s"${indent}NVal(v: $v)"
            head

          case XC(ret, send) =>
            val head = s"${indent}XC(v: ${t.value}, send: $send)"
            head

          case Call(id, right) =>
            val head = s"${indent}Call(id: $id, v: ${t.value})"
            val r = loop(right, level + 1)
            s"$head\n$r"

          case Next(first, last) =>
            val head = s"${indent}Next(v: ${t.value})"
            val f = loop(first, level + 1)
            val l = loop(last, level + 1)
            s"$head\n$f\n$l"

      loop(this, 0)

  extension [A](t: AlignmentTree[A])
    def value: A =
      t match
        case AlignmentTree.Val(v)         => v
        case AlignmentTree.XC(ret, _)     => ret
        case AlignmentTree.Call(_, right) => right.value
        case AlignmentTree.Next(_, right) => right.value
    def children: Seq[AlignmentTree[Any]] =
      t match
        case AlignmentTree.Val(value)        => Seq()
        case AlignmentTree.XC(ret, send)     => Seq()
        case AlignmentTree.Call(id, right)   => Seq(right)
        case AlignmentTree.Next(left, right) => Seq(left, right)

  opaque type Env[D] = Map[D, AlignmentTree[Any]]
  extension [D, A](env: Env[D])
    private def alignWith(node: Alignment[D, A]): Env[D] =
      env.filter((_, t) =>
        (t, node) match
          case (AlignmentTree.Call(id1, _), Grammar.Call(id2, _)) =>
            id1 == id2
          case _ => true
      )
    def toMap: Map[D, AlignmentTree[Any]] =
      env

    private def enterChildN(n: Int): Env[D] =
      env.view.mapValues(_.children(n)).toMap

  object Env:
    def fromMap[D](m: Map[D, AlignmentTree[Any]]): Env[D] =
      m

  extension [D, A](fa: Alignment[D, A])
    def run(unsafeEnv: Env[D]): AlignmentTree[A] =
      val env: Env[D] = unsafeEnv.alignWith(fa)
      fa match
        case AlignedContext(f) =>
          AlignmentTree.Val(f(env.asInstanceOf[Env[D]]))
        case Pure(a) =>
          AlignmentTree.Val(a)
        case Call(id, f) =>
          val runA = f().run(env.enterChildN(0))
          AlignmentTree.Call(id, runA)
        case XC(ret, send) =>
          AlignmentTree.XC(ret, send)
        case FlatMap(fa, f) =>
          val left = fa.run(env.enterChildN(0))
          val right = f(left.value).run(env.enterChildN(1))
          AlignmentTree.Next(left, right)

object Test:
  import aggregate.NValues.*
  import AlignmentModule.*
  import aggregate.AggregateAPI.Device
  import aggregate.AggregateAPI.Input

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
  def sensor[A](name: Aggregate[String]): Aggregate[A] =
    Sensor(name)
  def uid: Aggregate[Device] =
    Uid
  extension [A](a: Aggregate[A])
    def self: Aggregate[A] =
      Self(a)

    def overrideDevice(d: Aggregate[Device], f: A => A): Aggregate[A] =
      OverrideDevice(a, d, f)

    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] =
      a.flatMap(a => pureNV(f(a)))
    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B] =
      FlatMap(a, f)

  def pure[A](a: A): Aggregate[A] =
    pureNV(NValue(a))
  def pureNV[A](a: NValue[A]): Aggregate[A] =
    Pure(a)

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
      val _uid = input.uid
      val _sensors = input.sensors
      a match
        case Exchange(default, body) =>
          for
            default <- default.toAlignment
            defaultValue = default(_uid)
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
              val neighbours = ctx.toMap.keySet - _uid
              NValue(neighbours.foldLeft(init(_uid))((res, d) => f(res, a(d))))
            )
          yield res
        case Mux(cond, th, el) =>
          for
            cond <- cond.toAlignment
            th <- th.toAlignment
            el <- el.toAlignment
          yield if cond(_uid) then th else el
        case Call(id, f) =>
          for
            f <- f.toAlignment
            res <- Alignment.call(id, () => f(_uid)().toAlignment)
          yield res
        case Sensor(name) =>
          for name <- name.toAlignment
          yield _sensors(name(_uid)).asInstanceOf[NValue[A]]
        case Uid =>
          Alignment.pure(NValue(_uid))
        case Self(a) =>
          for a <- a.toAlignment
          yield NValue(a(_uid))
        case OverrideDevice(a, d, f) =>
          for
            a <- a.toAlignment
            d <- d.toAlignment
          yield a.setWith(d(_uid), f)
        case FlatMap(a, f) =>
          import AlignmentModule.flatMap as fm
          // a.fm(f) // Does not work
          a.toAlignment.fm(f(_).toAlignment)
        case Pure(nvalues) =>
          Alignment.pure(nvalues)
        case Branch(cond, th, el) =>
          for
            cond <- cond.toAlignment
            condition = cond(_uid)
            id = s"branch-$condition"
            res <- Alignment.call(
              id,
              () => if condition then th.toAlignment else el.toAlignment
            )
          yield res
