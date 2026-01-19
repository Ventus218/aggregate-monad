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
    case OverrideDevice[A](fa: Aggregate[A], d: Aggregate[Device], f: A => A)
        extends Aggregate[A]
    case Pure(nvalues: NValue[A])
    case FlatMap[A, B](a: Aggregate[A], f: NValue[A] => Aggregate[B])
        extends Aggregate[B]

    // TODO: here just until we can implement call
    case Branch(cond: Aggregate[Boolean], th: Aggregate[A], el: Aggregate[A])

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

  def branch[A](cond: Aggregate[Boolean])(th: Aggregate[A])(
      el: Aggregate[A]
  ): Aggregate[A] =
    Branch(cond, th, el)

  def uid: Aggregate[Device] = Uid

  extension [A](fa: Aggregate[A])
    def self: Aggregate[A] = Self(fa)
    def update(d: Aggregate[Device], f: A => A): Aggregate[A] =
      OverrideDevice(fa, d, f)

  extension [A](fa: Aggregate[A])
    def map[B](f: NValue[A] => NValue[B]): Aggregate[B] =
      fa.flatMap(a => Pure(f(a)))

    def flatMap[B](f: NValue[A] => Aggregate[B]): Aggregate[B] = FlatMap(fa, f)

  def pureGiven[A]: Conversion[A, Aggregate[A]] = x => Pure(NValue(x))
  def nvalGiven[A]: Conversion[NValue[A], Aggregate[A]] = x => Pure(x)

  case class Input(
      uid: Device,
      sensors: Map[String, NValue[Any]]
  )

  extension [A](nv: NValue[A])
    private def selfValue(using input: Input): A =
      nv(input.uid)

  import aggregate.nonfree.Env.*
  import scala.language.implicitConversions

  private given [A]: Conversion[Grammar[A], Env.TreeNodeType] with
    def apply(x: Grammar[A]): Env.TreeNodeType =
      x match
        case Call(id, _)     => Env.TreeNodeType.Call(id)
        case Exchange(_, _)  => Env.TreeNodeType.XC
        case FlatMap(_, _)   => Env.TreeNodeType.Sequence
        case Branch(_, _, _) => Env.TreeNodeType.Sequence
        case _               => Env.TreeNodeType.NVal

  extension [A](a: Aggregate[A])
    private def runAsChildN(n: Int)(using Env, Input): ValueTree[A] =
      a.run(using summon[Env].enterChildN(n))

    def run(using envUnsafe: Env, input: Input): ValueTree[A] =
      given env: Env = envUnsafe.alignWith(a)
      a match
        case Pure(nvalues) =>
          // TODO: here i could "clean" nvalues from non aligned devices (if necessary)
          // val map = env.alignedDevices.map(d => (d, nvalues(d))).toMap
          // val cleanNvalue = NValue(nvalues.default, map)
          ValueTree.nval(nvalues)

        case Uid =>
          ValueTree.nval(NValue(input.uid))

        case Self(a) =>
          val aTree = a.runAsChildN(0)
          ValueTree.nval(NValue(aTree.nv.selfValue), aTree)

        case OverrideDevice(a, d, f) =>
          val aTree = a.runAsChildN(0)
          val dTree = d.runAsChildN(1)
          val updatedNValue = aTree.nv.setWith(dTree.nv(input.uid), f)
          ValueTree.nval(updatedNValue, aTree, dTree)

        case Sensor(name) =>
          val nameTree = name.runAsChildN(0)
          val sensorNValue =
            input.sensors(nameTree.nv.selfValue).asInstanceOf[NValue[A]]
          ValueTree.nval(sensorNValue, nameTree)

        case Mux(cond, th, el) =>
          val condTree = cond.runAsChildN(0)
          val thTree = th.runAsChildN(1)
          val elTree = el.runAsChildN(2)
          val result = if condTree.nv.selfValue then thTree.nv else elTree.nv
          ValueTree.nval(result, condTree, thTree, elTree)

        case FlatMap(a, f) =>
          val aTree = a.runAsChildN(0)
          val bTree = f(aTree.nv).runAsChildN(1)
          ValueTree.seq(aTree, bTree)

        case NFold(init, a, f) =>
          val initTree = init.runAsChildN(0)
          val aTree = a.runAsChildN(1)
          val res = env.alignedDevices
            .filterNot(_ == input.uid)
            .foldLeft(initTree.nv.selfValue)((res, d) => f(res, aTree.nv(d)))
          ValueTree.nval(NValue(res), initTree, aTree)

        case Exchange(default, body) =>
          val defaultTree = default.runAsChildN(0)
          val defaultValue = defaultTree.nv.selfValue
          val overrides =
            env.toMap.map((d, vt) =>
              (d, vt.nv(d).asInstanceOf[defaultValue.type])
            )
          val (ret, send) = body(Pure(NValue(defaultValue, overrides)))
          val retTree = ret.runAsChildN(1)
          val sendTree = send.runAsChildN(2)
          // not sure about what trees include in children
          ValueTree.xc(retTree.nv, sendTree.nv, defaultTree, retTree, sendTree)

        case Call(id, f) =>
          val fTree = f.runAsChildN(0)
          val fun = fTree.nv.selfValue()
          val funTree = fun.runAsChildN(1)
          ValueTree.call(id, funTree.nv, fTree, funTree)

        // TODO: Remove once we can implement it with call
        case Branch(cond, th, el) =>
          val condTree = cond.runAsChildN(0)
          val condition = condTree.nv.selfValue
          val id = s"branch-$condition"
          val call = Call(id, pureGiven(() => if condition then th else el))
          ValueTree.seq(condTree, call.runAsChildN(1))
