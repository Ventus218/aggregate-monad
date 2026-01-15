package aggregate.nonfree

import aggregate.AggregateAPI.Device
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
    case Mux(cond: Aggregate[Boolean], th: Aggregate[A], el: Aggregate[A])
    case Call(id: String, f: Aggregate[() => Aggregate[A]])
    case Sensor(name: Aggregate[String])
    case Uid extends Aggregate[Device]
    case Self(a: Aggregate[A])
    // More generally this will become UpdateDevice and then we can implement this through Map
    case UpdateSelf[A](fa: Aggregate[A], f: A => A) extends Aggregate[A]
    case Pure(nvalues: NValue[A])
    case Map_[A, B](a: Aggregate[A], f: A => B) extends Aggregate[B]
    case PointwiseOp[A, B](a: Aggregate[A], b: Aggregate[A], f: (A, A) => B)
        extends Aggregate[B]

  import Grammar.*

  def sensor[A](name: Aggregate[String]): Aggregate[A] =
    Sensor(name)

  def call[A](f: Aggregate[() => Aggregate[A]]): Aggregate[A] = Call(???, f)

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
      sensors: Map[String, NValue[Any]]
  )

  extension [A](nv: NValue[A])
    def selfValue(using input: Input): A =
      nv(input.uid)

  private def uid(using input: Input): Device =
    input.uid

  import aggregate.nonfree.Env.*
  import scala.language.implicitConversions

  private given [A]: Conversion[Grammar[A], Env.TreeNodeType] with
    def apply(x: Grammar[A]): Env.TreeNodeType =
      x match
        case Call(id, _)    => Env.TreeNodeType.Call(id)
        case Exchange(_, _) => Env.TreeNodeType.XC
        case _              => Env.TreeNodeType.NVal

  extension [A](a: Aggregate[A])
    def run(env: Env)(using input: Input): ValueTree[A] =
      // I think we can safely assume that all devices are aligned with the root node
      a match
        case Exchange(default, body) =>
          val defaultTree = default.run(env.alignWithChild(0, default))
          val defaultValue = defaultTree.nv.selfValue
          val overrides =
            env.toMap.map((d, vt) =>
              (d, vt.nv(d).asInstanceOf[defaultValue.type])
            )
          val (ret, send) = body(Pure(NValue(defaultValue, overrides)))
          val retTree = ret.run(env.alignWithChild(1, ret))
          val sendTree = send.run(env.alignWithChild(2, send))
          // not sure about what trees include in children
          ValueTree.xc(retTree.nv, sendTree.nv, defaultTree, retTree, sendTree)
        case Call(id, f) =>
          val fTree = f.run(env.alignWithChild(0, f))
          val fun = fTree.nv.selfValue()
          val funTree = fun.run(env.alignWithChild(1, fun))
          ValueTree.call(id, funTree.nv, fTree, funTree)
        case NFold(init, a, f) =>
          val initTree = init.run(env.alignWithChild(0, init))
          val aTree = a.run(env.alignWithChild(1, a))
          val res = env.alignedDevices
            .filterNot(_ == uid)
            .foldLeft(initTree.nv.selfValue)((res, d) => f(res, aTree.nv(d)))
          ValueTree.nval(NValue(res), initTree, aTree)
        case Mux(cond, th, el) =>
          val condTree = cond.run(env.alignWithChild(0, cond))
          val thTree = th.run(env.alignWithChild(1, th))
          val elTree = el.run(env.alignWithChild(2, el))
          val result = if condTree.nv.selfValue then thTree.nv else elTree.nv
          ValueTree.nval(result, condTree, thTree, elTree)
        case Sensor(name) =>
          val nameTree = name.run(env.alignWithChild(0, name))
          val sensorNValue =
            input.sensors(nameTree.nv.selfValue).asInstanceOf[NValue[A]]
          ValueTree.nval(sensorNValue, nameTree)
        case Uid =>
          ValueTree.nval(NValue(uid))
        case Self(a) =>
          val aTree = a.run(env.alignWithChild(0, a))
          ValueTree.nval(NValue(aTree.nv.selfValue), aTree)
        case UpdateSelf(a, f) =>
          val aTree = a.run(env.alignWithChild(0, a))
          val updatedNValue = aTree.nv.setWith(uid, f)
          ValueTree.nval(updatedNValue, aTree)
        case Pure(nvalues) =>
          ValueTree.nval(nvalues)
        case Map_(a, f) =>
          val aTree = a.run(env.alignWithChild(0, a))
          ValueTree.nval(aTree.nv.map(f), aTree)
        case PointwiseOp(a, b, f) =>
          val aTree = a.run(env.alignWithChild(0, a))
          val aNV = aTree.nv
          val bTree = b.run(env.alignWithChild(1, b))
          val bNV = bTree.nv
          val values = env.alignedDevices
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
