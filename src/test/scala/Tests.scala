package aggregate.nonfree

import scala.language.implicitConversions
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import aggregate.AggregateAPI.{given, *}
import aggregate.AggregateLib.*
import aggregate.NValues.*

class Test extends org.scalatest.funsuite.AnyFunSuite:

  val d1: Device = Device.fromInt(1)
  val d2: Device = Device.fromInt(2)
  val d3: Device = Device.fromInt(3)
  val d4: Device = Device.fromInt(4)

  test("pointwise on NValues"):
    val nva = NValue(3, Map((d1 -> 3), (d2 -> 0)))
    val nvb = NValue(1, Map((d1 -> 1), (d3 -> 3)))
    val nvc = for
      a <- nva
      b <- nvb
    yield a + b

    nvc(d1) shouldBe 4
    nvc(d2) shouldBe 1
    nvc(d3) shouldBe 6
    nvc(d4) shouldBe 4

  test("pure nvalue"):
    val program: Aggregate[Int] = 1
    val input = Input(d1, Map())
    val env = Env(Map())
    val vt = program.run(using env, input)
    vt.nv(d1) shouldBe 1
    vt.nv(d2) shouldBe 1
    vt.nv(d3) shouldBe 1
    vt.nv(d4) shouldBe 1

  test("mux"):
    val cond = NValue(true, Map((d1 -> true), (d2 -> false)))
    val program = mux(cond)(1)(0)
    val env = Env(Map())

    program.run(using env, Input(d1, Map())).nv shouldBe NValue(1)
    program.run(using env, Input(d2, Map())).nv shouldBe NValue(0)

  // TODO: ask
  test("mux2"):
    val cond = NValue(true, Map((d1 -> true), (d2 -> false)))
    val trueNVal = NValue(1, Map((d1 -> 9)))
    val program = mux(cond)(nvalGiven(trueNVal))(0)
    val env = Env(Map())

    program.run(using env, Input(d1, Map())).nv shouldBe trueNVal
    program.run(using env, Input(d2, Map())).nv shouldBe NValue(0)

  test("alignment"):
    val program = nfold(0)(1)(_ + _) // Count neighbours
    val input = Input(d1, Map())

    // We'll use this vt for creating fake envs
    val vt = program.run(using Env(Map()), input)

    val zeroNeighboursEnv = Env(Map())
    val oneNeighbourEnv = Env(((d2 -> vt)))
    val twoNeighboursEnv = Env(
      (d2 -> vt),
      (d3 -> vt)
    )
    val threeNeighboursEnv = Env(
      (d2 -> vt),
      (d3 -> vt),
      (d4 -> vt)
    )
    program.run(using zeroNeighboursEnv, input).nv(d1) shouldBe 0
    program.run(using oneNeighbourEnv, input).nv(d1) shouldBe 1
    program.run(using twoNeighboursEnv, input).nv(d1) shouldBe 2
    program.run(using threeNeighboursEnv, input).nv(d1) shouldBe 3

  test("exchange"):
    val program = exchange(0)(n => retsend(n + 1))

    val d1vt0 = program.run(using Env(), Input(uid = d1, Map()))

    d1vt0.nv(d1) shouldBe 1
    d1vt0.nv(d2) shouldBe 1

    val d2vt0 =
      program.run(using Env(Map((d1 -> d1vt0))), Input(uid = d2, Map()))

    d2vt0.nv(d1) shouldBe 2
    d2vt0.nv(d2) shouldBe 1

    val d1vt1 = program.run(using
      Env(Map((d1 -> d1vt0), (d2 -> d2vt0))),
      Input(uid = d1, Map())
    )

    d1vt1.nv(d1) shouldBe 2
    d1vt1.nv(d2) shouldBe 2

  test("nfold"):
    def countAlignedNeighbours: Aggregate[Int] =
      nfold(init = 0)(1)(_ + _)

    val d1vt0 =
      countAlignedNeighbours.run(using Env(Map()), Input(uid = d1, Map()))
    val d2vt0 =
      countAlignedNeighbours.run(using Env(Map()), Input(uid = d2, Map()))
    val d3vt0 =
      countAlignedNeighbours.run(using Env(Map()), Input(uid = d3, Map()))

    d1vt0.nv(d1) shouldBe 0
    d2vt0.nv(d2) shouldBe 0
    d3vt0.nv(d3) shouldBe 0

    val d1vt1 = countAlignedNeighbours.run(using
      Env(Map((d1 -> d1vt0), (d2 -> d2vt0), (d3 -> d3vt0))),
      Input(uid = d1, Map())
    )
    val d2vt1 = countAlignedNeighbours.run(using
      Env(Map((d1 -> d1vt0), (d2 -> d2vt0), (d3 -> d3vt0))),
      Input(uid = d2, Map())
    )
    val d3vt1 = countAlignedNeighbours.run(using
      Env(Map((d1 -> d1vt0), (d3 -> d3vt0))),
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

    val env = Env(Map((d1 -> d1vt0), (d2 -> d2vt0)))
    val d1vt1 = program.run(using env, Input(uid = d1, Map()))
    val d2vt1 = program.run(using env, Input(uid = d2, Map()))

    d1vt1.nv(d1) shouldBe 0
    d2vt1.nv(d2) shouldBe 1

  test("branch and alignment"):
    def countAlignedChild: Aggregate[Int] =
      nfold(init = 0)(1)(_ + _)
    def sens = NValue(false, Map((d1 -> true), (d2 -> false)))

    val program1 = branch(sens)(countAlignedChild)(countAlignedChild)

    val p1d1vt0 = program1.run(using Env(Map()), Input(uid = d1, Map()))
    val p1d2vt0 = program1.run(using Env(Map()), Input(uid = d2, Map()))

    val env1 = Env(Map((d1 -> p1d1vt0), (d2 -> p1d2vt0)))

    val p1d1vt1 = program1.run(using env1, Input(uid = d1, Map()))
    val p1d2vt1 = program1.run(using env1, Input(uid = d2, Map()))

    p1d1vt1.nv(d1) shouldBe 0
    p1d2vt1.nv(d2) shouldBe 0

    val program2 = for
      count <- countAlignedChild
      res <- branch(sens)(nvalGiven(count))(count)
    yield res

    val p2d1vt0 = program2.run(using Env(Map()), Input(uid = d1, Map()))
    val p2d2vt0 = program2.run(using Env(Map()), Input(uid = d2, Map()))

    val env2 = Env(Map((d1 -> p2d1vt0), (d2 -> p2d2vt0)))

    val p2d1vt1 = program2.run(using env2, Input(uid = d1, Map()))
    val p2d2vt1 = program2.run(using env2, Input(uid = d2, Map()))

    p2d1vt1.nv(d1) shouldBe 1
    p2d2vt1.nv(d2) shouldBe 1

  test("question about nvalues"):
    extension [A: Numeric](nv1: NValue[A])
      infix def +(nv2: NValue[A]) =
        import Numeric.Implicits.*
        NValue(
          nv1.default + nv2.default,
          (nv1.values.keySet ++ nv2.values.keySet)
            .map(d => (d -> (nv1(d) + nv2(d))))
            .toMap
        )

    // Imagine i am device d1 ...
    val cond: Aggregate[Boolean] = NValue(true, Map((d4 -> false)))
    val a: Aggregate[Int] = NValue(0, Map((d2 -> 2), (d3 -> 4)))
    val b: Aggregate[Int] = NValue(0, Map((d4 -> 1)))
    for
      a <- a
      b <- b
      // sum = a + b // Why fails compiling?
      sum <- nvalGiven(a + b)
      // Here sum is 0[d2 -> 2, d3 -> 4, d4 -> 1]
      res <-
        exchange(0)(n =>
          retsend:
            branch(cond) {
              // Here i am aligned only with d2 and d3
              // Should sum be
              // 0[d2 -> 2, d3 -> 4]
              // OR
              // 0[d2 -> 2, d3 -> 4, d4 -> 1]
              // And also do i send a value also to unaligned neighbours?
              // They may become aligned in their next round..
              nvalGiven(sum)
            } {
              n
            }
        )
    yield res
