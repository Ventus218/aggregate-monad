package aggregate.simple

import aggregate.AggregateAPI
import aggregate.AggregateAPI.Device
import aggregate.NValues.NValue

object SimpleEffect:
  opaque type Aggregate[+A] = AggregateImpl[A]
  private enum AggregateImpl[+A]:
    case Exchange[A, S](
        default: Aggregate[S],
        body: Aggregate[S] => (Aggregate[A], Aggregate[S])
    ) extends Aggregate[A]
    case NFold[A, B](init: Aggregate[A], a: Aggregate[B], f: (A, B) => A)
        extends Aggregate[A]
    case Mux(cond: Aggregate[Boolean], th: Aggregate[A], el: Aggregate[A])
    case Call(f: Aggregate[() => Aggregate[A]])
    case Sensor(name: Aggregate[String])
    case Uid extends Aggregate[Device]
    case Self(a: Aggregate[A])
    // More generally this will become UpdateDevice and then we can implement this through Map
    case UpdateSelf[A](fa: Aggregate[A], f: A => A) extends Aggregate[A]
    case Pure(nvalues: NValue[A])
    case Map_[A, B](a: Aggregate[A], f: A => B) extends Aggregate[B]
    case PointwiseOp[A, B](a: Aggregate[A], b: Aggregate[A], f: (A, A) => B)
        extends Aggregate[B]

  import AggregateImpl.*

  def sensor[A](name: Aggregate[String]): Aggregate[A] =
    Sensor(name)

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = Call(f)

  def exchange[A, S](default: Aggregate[S])(
      f: Aggregate[S] => (Aggregate[A], Aggregate[S])
  ): Aggregate[A] = Exchange(default, f)

  def nfold[A, B](init: Aggregate[A])(a: Aggregate[B])(
      f: (A, B) => A
  ): Aggregate[A] = NFold(init, a, f)

  def mux[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] =
    Mux(cond, th, el)

  def uid: Aggregate[Device] = Uid

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A] = Self(fa)
    def updateSelf(f: A => A): Aggregate[A] = UpdateSelf(fa, f)

  extension [A](fa: Aggregate[A])
    def map[B](f: A => B): Aggregate[B] = Map_(fa, f)

  def pointwise[A, B](
      a: Aggregate[A],
      b: Aggregate[A],
      f: (A, A) => B
  ): Aggregate[B] =
    PointwiseOp(a, b, f)

  def pureGiven[A]: Conversion[A, Aggregate[A]] = x => Pure(NValue(x))

  case class Input(
      uid: Device,
      sensors: Map[String, NValue[Any]],
      messages: Map[Device, ValueTree[Any]]
  )

  enum ValueTree[+A]:
    case NVal(nv: NValue[A], children: Seq[ValueTree[Any]])
    case XC[+R, +S](
        ret: NValue[R],
        send: NValue[S],
        children: Seq[ValueTree[Any]]
    ) extends ValueTree[R]
    case Call(id: String, nv: NValue[A], children: Seq[ValueTree[Any]])

  extension [A](vt: ValueTree[A])
    def nv: NValue[A] = vt match
      case ValueTree.NVal(nv, children)      => nv
      case ValueTree.XC(ret, send, children) => nv
      case ValueTree.Call(id, nv, children)  => nv
    def children: Seq[ValueTree[Any]] = vt match
      case ValueTree.NVal(nv, children)      => children
      case ValueTree.XC(ret, send, children) => children
      case ValueTree.Call(id, nv, children)  => children

  object ValueTree:
    def nval[A](nv: NValue[A], children: ValueTree[Any]*): ValueTree.NVal[A] =
      ValueTree.NVal(nv, children)

    def xc[R, S](
        nv: NValue[R],
        send: NValue[S],
        children: ValueTree[Any]*
    ): ValueTree.XC[R, S] = ValueTree.XC(nv, send, children)

    def call[A](
        id: String,
        nv: NValue[A],
        children: ValueTree[Any]*
    ): ValueTree.Call[A] = ValueTree.Call(id, nv, children)

  extension [A](nv: NValue[A])
    def selfValue(using input: Input): A =
      nv(input.uid)

  extension [A](a: Aggregate[A])
    def uid(using input: Input): Device =
      input.uid

    def run(using input: Input): ValueTree[A] =
      a match
        case Exchange(default, body) =>
          val defaultTree = default.run
          val defaultValue = defaultTree.nv.selfValue
          // TODO:
          val neighbourMessages: NValue[defaultValue.type] = ???
          val (ret, send) = body(Pure(neighbourMessages))
          val retTree = ret.run
          val sendTree = send.run
          // not sure about what trees include in children
          ValueTree.xc(retTree.nv, sendTree.nv, defaultTree, retTree, sendTree)
        case Call(f) =>
          val fTree = f.self.run
          val fun = fTree.nv.selfValue
          val funTree = fun().run
          // TODO:
          val id: String = ???
          ValueTree.call(id, funTree.nv, funTree)
        case NFold(init, a, f) =>
          val initTree = init.self.run
          val aTree = a.run
          // TODO:
          val alignedDevices: Seq[Device] = ???
          val res = alignedDevices.foldLeft(initTree.nv.selfValue)((res, d) =>
            f(res, aTree.nv(d))
          )
          ValueTree.nval(NValue(res), initTree, aTree)
        case Mux(cond, th, el) =>
          val condTree = cond.self.run
          val thTree = th.run
          val elTree = el.run
          val result = if condTree.nv.selfValue then thTree.nv else elTree.nv
          ValueTree.nval(result, condTree, thTree, elTree)
        case Sensor(name) =>
          val nameTree = name.run
          val sensorNValue =
            input.sensors(nameTree.nv.selfValue).asInstanceOf[NValue[A]]
          ValueTree.nval(sensorNValue, nameTree)
        case Uid =>
          ValueTree.nval(NValue(uid))
        case Self(a) =>
          val aTree = a.run
          ValueTree.nval(NValue(aTree.nv.selfValue), aTree)
        case UpdateSelf(a, f) =>
          val aTree = a.run
          val updatedNValue = aTree.nv.setWith(uid, f)
          ValueTree.nval(updatedNValue, aTree)
        case Pure(nvalues) =>
          ValueTree.nval(nvalues)
        case Map_(a, f) =>
          val aTree = a.run
          ValueTree.nval(aTree.nv.map(f), aTree)
        case PointwiseOp(a, b, f) =>
          val aTree = a.run
          val aNV = aTree.nv
          val bTree = b.run
          val bNV = bTree.nv
          // TODO:
          val alignedDevices: Seq[Device] = ???
          val values = alignedDevices
            .map(d => (d -> (aNV(d), bNV(d))))
            .toMap
            .view
            .mapValues((a, b) => f(a, b))
            .toMap

          ValueTree.nval(
            NValue(f(aNV.default, bNV.default), values),
            aTree,
            bTree
          )
