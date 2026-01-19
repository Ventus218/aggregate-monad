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

  test("pointwise on NValues"):
    val nva = NValue(3, Map((uid -> 3), (d2 -> 0)))
    val nvb = NValue(1, Map((uid -> 1), (d3 -> 3)))
    val nvc = for
      a <- nva
      b <- nvb
    yield a + b

    nvc(uid) shouldBe 4
    nvc(d1) shouldBe 4
    nvc(d2) shouldBe 1
    nvc(d3) shouldBe 6
    nvc(d4) shouldBe 4

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

  test("mux"):
    val cond = NValue(true, Map((d1 -> true), (d2 -> false)))
    val program = mux(cond)(1)(0)
    val env = Env(Map())

    val d1vt = program.run(using env, Input(d1, Map()))
    d1vt.nv shouldBe NValue(1)
    val d2vt = program.run(using env, Input(d2, Map()))
    d2vt.nv shouldBe NValue(0)

  // TODO: ask
  test("mux2"):
    val cond = NValue(true, Map((d1 -> true), (d2 -> false)))
    val trueNVal = NValue(1, Map((d1 -> 9)))
    val program = mux(cond)(nvalGiven(trueNVal))(0)
    val env = Env(Map())

    val d1vt = program.run(using env, Input(d1, Map()))
    d1vt.nv shouldBe trueNVal
    val d2vt = program.run(using env, Input(d2, Map()))
    d2vt.nv shouldBe NValue(0)

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

  test("nfold"):
    def countAlignedChild: Aggregate[Int] =
      nfold(init = 0)(1)(_ + _)

    val d1vt0 = countAlignedChild.run(using Env(Map()), Input(uid = d1, Map()))
    val d2vt0 = countAlignedChild.run(using Env(Map()), Input(uid = d2, Map()))
    val d3vt0 = countAlignedChild.run(using Env(Map()), Input(uid = d3, Map()))

    d1vt0.nv(d1) shouldBe 0
    d2vt0.nv(d2) shouldBe 0
    d3vt0.nv(d3) shouldBe 0

    val d1vt1 = countAlignedChild.run(using
      Env(Map((d2 -> d2vt0), (d3 -> d3vt0))),
      Input(uid = d1, Map())
    )
    val d2vt1 = countAlignedChild.run(using
      Env(Map((d1 -> d1vt0), (d3 -> d3vt0))),
      Input(uid = d2, Map())
    )
    val d3vt1 = countAlignedChild.run(using
      Env(Map((d1 -> d1vt0))),
      Input(uid = d3, Map())
    )

    d1vt1.nv(d1) shouldBe 2
    d2vt1.nv(d2) shouldBe 2
    d3vt1.nv(d3) shouldBe 1

  test("branch"):
    def sens = NValue(false, Map((d1 -> true), (d2 -> false)))
    val program = branch(sens)(0)(1)

    val d1vt0 = program.run(using Env(Map()), Input(uid = d1, Map()))
    val d2vt0 = program.run(using Env(Map()), Input(uid = d2, Map()))

    val d1vt1 =
      program.run(using Env(Map((d2 -> d2vt0))), Input(uid = d1, Map()))
    val d2vt1 =
      program.run(using Env(Map((d1 -> d1vt0))), Input(uid = d2, Map()))

    d1vt1.nv(d1) shouldBe 0
    d2vt1.nv(d2) shouldBe 1

  test("branch and alignment"):
    def countAlignedChild: Aggregate[Int] =
      nfold(init = 0)(1)(_ + _)
    def sens = NValue(false, Map((d1 -> true), (d2 -> false)))

    val program1 = branch(sens)(countAlignedChild)(countAlignedChild)

    val p1d1vt0 = program1.run(using Env(Map()), Input(uid = d1, Map()))
    val p1d2vt0 = program1.run(using Env(Map()), Input(uid = d2, Map()))

    val p1d1vt1 =
      program1.run(using Env(Map((d2 -> p1d2vt0))), Input(uid = d1, Map()))
    val p1d2vt1 =
      program1.run(using Env(Map((d1 -> p1d1vt0))), Input(uid = d2, Map()))

    p1d1vt1.nv(d1) shouldBe 0
    p1d2vt1.nv(d2) shouldBe 0

    val program2 = for
      count <- countAlignedChild
      res <- branch(sens)(nvalGiven(count))(count)
    yield res

    val p2d1vt0 = program2.run(using Env(Map()), Input(uid = d1, Map()))
    val p2d2vt0 = program2.run(using Env(Map()), Input(uid = d2, Map()))

    val p2d1vt1 =
      program2.run(using Env(Map((d2 -> p2d2vt0))), Input(uid = d1, Map()))
    val p2d2vt1 =
      program2.run(using Env(Map((d1 -> p2d1vt0))), Input(uid = d2, Map()))

    p2d1vt1.nv(d1) shouldBe 1
    p2d2vt1.nv(d2) shouldBe 1
