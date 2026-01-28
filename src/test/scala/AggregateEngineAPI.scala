package aggregate

import AggregateAPI.*
import NValues.*

trait AggregateEngineAPI:
  type Export[A]
  extension [A](e: Export[A]) def top: NValue[A]

  def newDevice(): Device
  def selfDevice: Device
  def round[A](a: Aggregate[A])(d: Device)(e: Env): Export[A]

trait AggregateEngine extends AggregateEngineAPI:
  import ValueTrees.*

  type Export[A] = ValueTree[A]
  extension [A](e: Export[A]) def top: NValue[A] = e.nv

  private var devices = 0
  def newDevice(): Device =
    devices += 1
    Device.fromInt(devices)
  def selfDevice: Device = Device.fromInt(0)

  def round[A](a: Aggregate[A])(d: Device)(e: Env): Export[A] =
    a.run(using d)(using e)

object AggregateEngineModule extends AggregateEngine
