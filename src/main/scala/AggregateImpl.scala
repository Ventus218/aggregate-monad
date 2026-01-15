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
      sensors: Map[String, NValue[Any]],
      messages: Map[Device, ValueTree[Any]]
  )

  extension [A](nv: NValue[A])
    def selfValue(using input: Input): A =
      nv(input.uid)

  extension (input: Input)
    def restrictToChildN(n: Int): Input =
      input.copy(messages = input.messages.view.mapValues(_.children(n)).toMap)

  extension [A](a: Aggregate[A])
    def uid(using input: Input): Device =
      input.uid

    def run(using unsafeInput: Input): ValueTree[A] =

      // keeping only messages of aligned devices
      given input: Input = unsafeInput.copy(messages =
        unsafeInput.messages
          .filter((_, t) =>
            // Since branches can only happen due to "Call"s i think it would
            // be enough to just check for those, but for now we check each
            // tree node.
            (t, a) match
              case (t: ValueTree.XC[?, ?], a: Exchange[?, ?]) => true
              case (ValueTree.Call(id1, _, _), Call(id2, _))  => id1 == id2
              case (t: ValueTree.NVal[?], a) =>
                !(a.isInstanceOf[Exchange[?, ?]] || a.isInstanceOf[Call[?]])
              case _ => false
          )
      )
      val alignedDevices = input.messages.keySet.toSeq

      a match
        case Exchange(default, body) =>
          val defaultTree = default.run(using input.restrictToChildN(0))
          val defaultValue = defaultTree.nv.selfValue
          val nvalue = NValue(
            defaultValue,
            alignedDevices
              .map(d =>
                (d -> input.messages(d).nv(d).asInstanceOf[defaultValue.type])
              )
              .toMap
          )
          val (ret, send) = body(Pure(nvalue))
          val retTree = ret.run(using input.restrictToChildN(1))
          val sendTree = send.run(using input.restrictToChildN(2))
          // not sure about what trees include in children
          ValueTree.xc(retTree.nv, sendTree.nv, defaultTree, retTree, sendTree)
        case Call(id, f) =>
          val fTree = f.run(using input.restrictToChildN(0))
          val fun = fTree.nv.selfValue
          val funTree = fun().run(using input.restrictToChildN(1))
          ValueTree.call(id, funTree.nv, fTree, funTree)
        case NFold(init, a, f) =>
          val initTree = init.run(using input.restrictToChildN(0))
          val aTree = a.run(using input.restrictToChildN(1))
          val res = alignedDevices
            .filterNot(_ == uid)
            .foldLeft(initTree.nv.selfValue)((res, d) => f(res, aTree.nv(d)))
          ValueTree.nval(NValue(res), initTree, aTree)
        case Mux(cond, th, el) =>
          val condTree = cond.run(using input.restrictToChildN(0))
          val thTree = th.run(using input.restrictToChildN(1))
          val elTree = el.run(using input.restrictToChildN(2))
          val result = if condTree.nv.selfValue then thTree.nv else elTree.nv
          ValueTree.nval(result, condTree, thTree, elTree)
        case Sensor(name) =>
          val nameTree = name.run(using input.restrictToChildN(0))
          val sensorNValue =
            input.sensors(nameTree.nv.selfValue).asInstanceOf[NValue[A]]
          ValueTree.nval(sensorNValue, nameTree)
        case Uid =>
          ValueTree.nval(NValue(uid))
        case Self(a) =>
          val aTree = a.run(using input.restrictToChildN(0))
          ValueTree.nval(NValue(aTree.nv.selfValue), aTree)
        case UpdateSelf(a, f) =>
          val aTree = a.run(using input.restrictToChildN(0))
          val updatedNValue = aTree.nv.setWith(uid, f)
          ValueTree.nval(updatedNValue, aTree)
        case Pure(nvalues) =>
          ValueTree.nval(nvalues)
        case Map_(a, f) =>
          val aTree = a.run(using input.restrictToChildN(0))
          ValueTree.nval(aTree.nv.map(f), aTree)
        case PointwiseOp(a, b, f) =>
          val aTree = a.run(using input.restrictToChildN(0))
          val aNV = aTree.nv
          val bTree = b.run(using input.restrictToChildN(1))
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
