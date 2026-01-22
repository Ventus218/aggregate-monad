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

  type Aggregate[A] = Device ?=> Alignment[Device, NValue[A]]

  // TODO:
  def _uid: Device ?=> Device = summon[Device]

  def exchange[A, S](
      default: Aggregate[S],
      body: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] =
    for
      default <- default
      defaultValue = default(_uid)
      nbrMessages <- Alignment.alignedContext[Device, NValue[S]](ctx =>
        val overrides =
          ctx.toMap.map((d, tree) =>
            (
              d,
              tree.value
                .asInstanceOf[NValue[A]](d)
                .asInstanceOf[defaultValue.type]
            )
          )
        NValue(defaultValue, overrides)
      )
      (ret, send) = body(Alignment.pure(nbrMessages))
      ret <- ret
      send <- send
      ret <- Alignment.xc(ret, send)
    yield ret

  def nFold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] =
    for
      init <- init
      a <- a
      res <- Alignment.alignedContext[Device, NValue[A]](ctx =>
        val neighbours = ctx.toMap.keySet - _uid
        NValue(neighbours.foldLeft(init(_uid))((res, d) => f(res, a(d))))
      )
    yield res
  def mux[A](
      cond: Aggregate[Boolean],
      th: Aggregate[A],
      el: Aggregate[A]
  ): Aggregate[A] =
    for
      cond <- cond
      th <- th
      el <- el
    yield if cond(_uid) then th else el
  def call[A](id: String, f: Aggregate[() => Aggregate[A]]): Aggregate[A] =
    for
      f <- f
      res <- Alignment.call(id, () => f(_uid)())
    yield res
  def sensor[A](name: Aggregate[String]): Aggregate[A] = ???
  def uid: Aggregate[Device] =
    pure(_uid)
  def self[A](a: Aggregate[A]): Aggregate[A] =
    for a <- a
    yield NValue(a(_uid))
  def overrideDevice[A](
      fa: Aggregate[A],
      d: Aggregate[Device],
      f: A => A
  ): Aggregate[A] = ???

  def pure[A](a: A): Aggregate[A] =
    pureNV(NValue(a))
  def pureNV[A](a: NValue[A]): Aggregate[A] =
    Alignment.pure(a)

  // TODO: just for the moment
  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] =
    for
      cond <- cond
      condition = cond(_uid)
      id = s"branch-$condition"
      res <- Alignment.call(id, () => if condition then th else el)
    yield res

  case class Input(uid: Device, sensors: Map[String, NValue[Any]])

  given conv[A]: Conversion[NValue[A], Aggregate[A]] with
    def apply(x: NValue[A]): Aggregate[A] = pureNV(x)
  import scala.language.implicitConversions

  val d0: Device = Device.fromInt(0)
  val d1: Device = Device.fromInt(1)
  val d2: Device = Device.fromInt(2)
  def countAlignedNeighbours: Aggregate[Int] =
    nFold(init = pure(0))(pure(1))(_ + _)
  def cond: Aggregate[Boolean] = conv(NValue(true, Map((d1 -> false))))

  def program: Aggregate[Int] =
    for
      count <- countAlignedNeighbours
      res <- branch(cond)(conv(count))(conv(count))
    yield res
  def program2: Aggregate[Int] =
    branch(cond)(countAlignedNeighbours)(countAlignedNeighbours)

  @main def main: Unit =
    def prog: Aggregate[Int] = program2
    val d0vt0 = prog(using d0).run(Env.fromMap(Map()))
    val d1vt0 = prog(using d1).run(Env.fromMap(Map()))
    val d2vt0 = prog(using d2).run(Env.fromMap(Map()))
    val env2 = Env.fromMap(Map((d0 -> d0vt0), (d1 -> d1vt0), (d2 -> d2vt0)))
    val d0vt1 = prog(using d0).run(env2)
    val d1vt1 = prog(using d1).run(env2)
    println(d0vt0)
    println()
    println(d1vt0)
    println()
    println(d0vt1)
    println()
    println(d1vt1)
