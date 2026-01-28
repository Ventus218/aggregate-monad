package aggregate

import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import scala.language.implicitConversions
import AggregateEngineModule.{*, given}
import Executor.DistributedSystem.platformSensor
import Executor.{*, given}
import AggregateAPI.*
import NValues.*

class AdvancedAggregateTest extends org.scalatest.funsuite.AnyFunSuite:
  extension [A](nv: NValue[A]) def asValue: A = nv(selfDevice)

  import AggregateLib.*

  test("gradient"):
    val place = Displacement.grid(3, 3)
    val ds = Platform()
      .withTopology(place.topology)
      .withSensor[Boolean](
        name = "src",
        values = Map(place((0, 0)) -> true),
        default = false)
      // nbrRange is a neighbour sensor, 1.0 everywhere except that device (1,0) perceives only device (0,0) at distance 1.1
      .withSensor[Double](
        name = "nbrRange",
        values = Map(place((1, 0)) -> NValue(1.0, Map(place((0,0)) -> 1.1))),
        default = 1.0)
      .asDistributedSystem:
        gradient(platformSensor("src"))(using platformSensor("nbrRange"))

    Seq(
      place((1, 0)) -> Double.PositiveInfinity,
      place((0, 0)) -> 0.0,
      place((1, 1)) -> Double.PositiveInfinity,
      place((1, 0)) -> 1.1,
      place((1, 1)) -> 2.1,
      place((2, 0)) -> 2.1,
      place((2, 1)) -> 3.1,
      place((0, 1)) -> 1.0
    ).foreach: (device, result) =>
      ds.fire(device).top.asValue shouldBe result
    (1 to 100) foreach (_ => ds.randomFire())
    ds.fire(place(2, 2)).top.asValue shouldBe 4.0

  test("broadcast"):
    import AggregateLib.*
    val place = Displacement.grid(3, 3)
    val ds = Platform()
      .withTopology(place.topology)
      .withSensor[Boolean](
        name = "src",
        values = Map(place((0, 0)) -> true),
        default = false)
      // nbrRange is a neighbour sensor, 1.0 everywhere except that device (1,0) perceives only device (0,0) at distance 1.1
      .withSensor[Double](
        name = "nbrRange",
        values = Map(place((1, 0)) -> NValue(1.0, Map(place((0, 0)) -> 1.1))),
        default = 1.0)
      .asDistributedSystem:
        broadcast(platformSensor("src"))(platformSensor("src"))(using platformSensor("nbrRange"))

    Seq(
      place((1, 0)) -> (Double.PositiveInfinity -> false),
      place((0, 0)) -> (0.0 -> true),
      place((1, 1)) -> (Double.PositiveInfinity -> false),
      place((1, 0)) -> (1.1 -> true),
      place((1, 1)) -> (2.1 -> true),
      place((2, 2)) -> (Double.PositiveInfinity -> false),
      place((2, 1)) -> (3.1 -> true),
    ).foreach: (device, result) =>
      ds.fire(device).top.asValue shouldBe result

  test("distanceBetween"):
    import AggregateLib.*
    val place = Displacement.grid(3, 3)
    val ds = Platform()
      .withTopology(place.topology)
      .withSensor[Boolean](
        name = "src",
        values = Map(place((0, 0)) -> true),
        default = false)
      .withSensor[Boolean](
        name = "dest",
        values = Map(place((2, 1)) -> true),
        default = false)
      .withSensor[Double](
        name = "nbrRange",
        values = Map(place((1, 0)) -> NValue(1.0, Map(place((0, 0)) -> 1.1))),
        default = 1.0)
      .asDistributedSystem:
        given range: Aggregate[Double] = platformSensor("nbrRange")
        distanceBetween(platformSensor("src"), platformSensor("dest"))

    (1 to 100) foreach (_ => ds.randomFire())
    place.topology.keys.foreach: device =>
      ds.fire(device).top.asValue shouldBe 3.0

  test("channel"):
    import AggregateLib.*
    val place = Displacement.grid(3, 3)
    val ds = Platform()
      .withTopology(place.topology)
      .withSensor[Boolean](
        name = "src",
        values = Map(place((1, 0)) -> true),
        default = false)
      .withSensor[Boolean](
        name = "dest",
        values = Map(place((1, 2)) -> true),
        default = false)
      .withSensor[Double](
        name = "nbrRange",
        values = Map(place((1, 0)) -> NValue(1.0, Map(place((0, 0)) -> 1.1))),
        default = 1.0)
      .asDistributedSystem:
        given range: Aggregate[Double] = platformSensor("nbrRange")
        channel(platformSensor("src"), platformSensor("dest"),0.3)

    (1 to 100) foreach (_ => ds.randomFire())
    place.topology.keys.toSet.filter(ds.fire(_).top.asValue).map(place(_)) shouldBe Set((1, 2), (1, 1), (1, 0))
