package aggregate.nonfree

import scala.language.implicitConversions
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import aggregate.AggregateAPI.{given, *}
import aggregate.AggregateLib.*
import aggregate.nonfree.AggregateImpl.Input
import aggregate.nonfree.Env.*
import aggregate.ValueTrees.*
import aggregate.NValues.*

class Test extends org.scalatest.funsuite.AnyFunSuite:

  val uid: Device = Device.fromInt(0)
  val d1: Device = Device.fromInt(1)
  val d2: Device = Device.fromInt(2)
  val d3: Device = Device.fromInt(3)
  val d4: Device = Device.fromInt(4)

  test("pure nvalue"):
    val program: Aggregate[Int] = 1
    val input = Input(uid, Map())
    val env = Env(Map())
    val vt = program.run(using env, input)
    vt.nv(uid) shouldBe 1
    vt.nv(d1) shouldBe 1
    vt.nv(d2) shouldBe 1
    vt.nv(d3) shouldBe 1
    vt.nv(d4) shouldBe 1

  test("alignment"):
    val program = nfold(0)(1)(_ + _) // Count neighbours
    val input = Input(uid, Map())

    // We'll use this vt for creating fake envs
    val vt = program.run(using Env(Map()), input)

    val zeroNeighboursEnv = Env(Map())
    val oneNeighbourEnv = Env(((d1 -> vt)))
    val twoNeighboursEnv = Env(
      (d1 -> vt),
      (d2 -> vt)
    )
    val threeNeighboursEnv = Env(
      (d1 -> vt),
      (d2 -> vt),
      (d3 -> vt)
    )
    program.run(using zeroNeighboursEnv, input).nv(uid) shouldBe 0
    program.run(using oneNeighbourEnv, input).nv(uid) shouldBe 1
    program.run(using twoNeighboursEnv, input).nv(uid) shouldBe 2
    program.run(using threeNeighboursEnv, input).nv(uid) shouldBe 3

  test("exchange"):
    val program = exchange(0)(n => retsend(n + 1))

    val vtRound1 = program.run(using Env(), Input(uid, Map()))

    vtRound1.nv(uid) shouldBe 1
    vtRound1.nv(d1) shouldBe 1
    vtRound1.nv(d2) shouldBe 1

    // TODO: continue...
