package alignment

object AlignmentModule:

  opaque type Alignment[D, +A] = Grammar[D, A]
  private enum Grammar[D, +A]:
    case Call(id: String, f: Alignment[D, A]) // TODO: what else here??
    case AlignedContext[D, A]() extends Alignment[D, Map[D, A]]
    case Pure[D, A](a: A) extends Grammar[D, A]
    case FlatMap[D, A, B](fa: Grammar[D, A], f: A => Grammar[D, B])
        extends Grammar[D, B]

  import Grammar.*

  object Alignment:
    def pure[D, A](a: A): Alignment[D, A] = Pure(a)
    def call[D, A](id: String, f: Alignment[D, A]): Alignment[D, A] =
      Call(id, f)
    def alignedContext[D, A]: Alignment[D, Map[D, A]] = AlignedContext()

  extension [D, A](fa: Alignment[D, A])
    def map[B](f: A => B): Alignment[D, B] =
      fa.flatMap(a => Alignment.pure(f(a)))
    def flatMap[B](f: A => Alignment[D, B]): Alignment[D, B] =
      FlatMap(fa, f)

  enum AlignmentTree[+A]:
    case Val(value: A)
    case Call(id: String, value: A, children: Seq[AlignmentTree[Any]])
    case Next(left: AlignmentTree[Any], right: AlignmentTree[A])

  extension [A](t: AlignmentTree[A])
    def value: A =
      t match
        case AlignmentTree.Val(v)         => v
        case AlignmentTree.Call(_, v, _)  => v
        case AlignmentTree.Next(_, right) => right.value

  opaque type Env[D] = Map[D, AlignmentTree[Any]]
  extension [D](env: Env[D])
    private def alignWith(node: Alignment[D, Any]): Env[D] =
      env.filter((_, t) =>
        // Since branches can only happen due to "Call"s i think it would
        // be enough to just check for those, but for now we check each
        // tree node.
        (t, node) match
          case (AlignmentTree.Call(id1, _, _), Grammar.Call(id2, _)) =>
            id1 == id2
          case _ => true
      )

  extension [D, A](fa: Alignment[D, A])
    def run(unsafeEnv: Env[D]): AlignmentTree[A] =
      val env: Env[D] = unsafeEnv.alignWith(fa)
      fa match
        case AlignedContext() =>
          ??? // TODO: what here???
        case Pure(a) =>
          AlignmentTree.Val(a)
        case Call(id, a) =>
          val runA = a.run(env) // TODO: check if env must me modified
          AlignmentTree.Call(id, runA.value, Seq(runA))
        case FlatMap(fa, f) =>
          val left = fa.run(env) // TODO: check if env must me modified
          val right =
            f(left.value).run(env) // TODO: check if env must me modified
          AlignmentTree.Next(left, right)

object Test:
  import aggregate.NValues.*
  import AlignmentModule.*
  import aggregate.AggregateAPI.Device

  type Aggregate[A] = Alignment[Device, NValue[A]]

  // TODO:
  def _uid: Device = ???

  def exchange[A, S](
      default: Aggregate[S],
      body: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] =
    for
      default <- default
      defaultValue = default(_uid)
      ctx <- Alignment.alignedContext[Device, NValue[A]]
      overrides = ctx.map((d, nv) => (d, nv(d).asInstanceOf[defaultValue.type]))
      (ret, send) = body(Alignment.pure(NValue(defaultValue, overrides)))
      ret <- ret
      send <- send
      _ <- Alignment.pure(send)
    yield ret

  def nFold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] =
    for
      init <- init
      a <- a
      ctx <- Alignment.alignedContext[Device, NValue[A]]
      devices = ctx.keySet
    yield NValue(
      (devices - _uid).foldLeft(init(_uid))((res, d) => f(res, a(d)))
    )
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
      res <- Alignment.call(id, f(_uid)())
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
    Alignment.pure(NValue(a))

  // TODO: just for the moment
  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] =
    for
      cond <- cond
      condition = cond(_uid)
      id = s"branch-condition"
      call <- Alignment.call(
        id,
        Alignment.pure(() => if condition then th else el)
      )
      res <- call()
    yield res

  case class Input(uid: Device, sensors: Map[String, NValue[Any]])

  def countAlignedNeighbours: Aggregate[Int] =
    nFold(init = pure(0))(pure(1))(_ + _)
  def cond: Aggregate[Boolean] = ???

  given [A]: Conversion[NValue[A], Aggregate[A]] = ???
  import scala.language.implicitConversions

  def program: Aggregate[Int] =
    for
      count <- countAlignedNeighbours
      res <- branch(cond)(count)(count)
    yield res
