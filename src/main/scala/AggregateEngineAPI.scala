package aggregate

import NValues.*
import AggregateAPI.*

/**
 * The facade to the engine implementation includes:
 *  - a trait with the contract (mixing in the API)
 *  - an implementing trait exporting from "core" implementation
 *  - an object to import from
 */

trait AggregateEngineAPI:
  type Export[A] 
  extension [A](e: Export[A]) def top: NValue[A]

  // def initialExport[A]: Export[A]

  def newDevice(): Device
  def selfDevice: Device
  def round[A](a: Aggregate[A])(d: Device)(e: Env): Export[A]

trait AggregateEngine extends AggregateEngineAPI :
  import ValueTrees.*

  type Export[A] = ValueTree[A]
  extension [A](e: Export[A]) def top: NValue[A] = e.nv
  // override def initialExport[A]: Export[A] = ValueTree.

  private var devices = 0
  def newDevice(): Device = 
    devices += 1
    Device.fromInt(devices)
  def selfDevice: Device = Device.fromInt(0)

  def round[A](a: Aggregate[A])(d: Device)(e: Env): Export[A] = 
    a.run(using e, Input(d, Map())) // TODO: sensors

object AggregateEngineModule extends AggregateEngine
