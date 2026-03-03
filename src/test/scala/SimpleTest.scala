package aggregate.nonfree

import scala.language.implicitConversions
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers.*
import aggregate.AggregateAPI.{given, *}
import aggregate.AggregateLib.*
import aggregate.ValueTrees.*
import aggregate.NValues.*

class SimpleTest extends org.scalatest.funsuite.AnyFunSuite:

  val d1: Device = Device.fromInt(1)
  val d2: Device = Device.fromInt(2)
  val d3: Device = Device.fromInt(3)
  val d4: Device = Device.fromInt(4)

  test("pointwise on NValues"):
    val nva = NValue(3, Map(d1 -> 3, d2 -> 0))
    val nvb = NValue(1, Map(d1 -> 1, d3 -> 3))
    val nvc = for
      a <- nva
      b <- nvb
    yield a + b

    nvc(d1) shouldBe 4
    nvc(d2) shouldBe 1
    nvc(d3) shouldBe 6
    nvc(d4) shouldBe 4

  test("add override to NValues"):
    val nv = NValue(3, Map(d1 -> 3, d2 -> 0)).set(d3, 6)

    nv(d1) shouldBe 3
    nv(d2) shouldBe 0
    nv(d3) shouldBe 6
    nv(d4) shouldBe 3

  test("pure nvalue"):
    val program: Aggregate[Int] = 1
    val vt = program.run(uid = d1)(Env())
    vt.nv(d1) shouldBe 1
    vt.nv(d2) shouldBe 1
    vt.nv(d3) shouldBe 1
    vt.nv(d4) shouldBe 1

  test("update value for device"):
    val a: Aggregate[Int] = NValue(0, d1 -> 1, d2 -> 2, d3 -> 3)
    val program = a.update(d2, _ => 0)
    val vt = program.run(uid = d1)(Env())
    vt.nv(d1) shouldBe 1
    vt.nv(d2) shouldBe 0
    vt.nv(d3) shouldBe 3
    vt.nv(d4) shouldBe 0

  test("update value for device - 2"):
    val a: Aggregate[Int] = NValue(0, d1 -> 1, d2 -> 2, d3 -> 3)
    val program = a.update(d2, _ + 2)
    val vt = program.run(uid = d1)(Env())
    vt.nv(d1) shouldBe 1
    vt.nv(d2) shouldBe 4
    vt.nv(d3) shouldBe 3
    vt.nv(d4) shouldBe 0

  test("update self"):
    val a: Aggregate[Int] = NValue(0, d1 -> 1, d2 -> 2, d3 -> 3)
    val program = a.updateSelf(_ + 2)
    val vt = program.run(uid = d1)(Env())
    vt.nv(d1) shouldBe 3
    vt.nv(d2) shouldBe 2
    vt.nv(d3) shouldBe 3
    vt.nv(d4) shouldBe 0

  test("update self - 2"):
    val a: Aggregate[Int] = NValue(0, d1 -> 1, d2 -> 2, d3 -> 3)
    val program = a.updateSelf(0)
    val vt = program.run(uid = d1)(Env())
    vt.nv(d1) shouldBe 0
    vt.nv(d2) shouldBe 2
    vt.nv(d3) shouldBe 3
    vt.nv(d4) shouldBe 0

  test("mux"):
    val cond = NValue(true, Map(d1 -> true, d2 -> false))
    val program = mux(cond)(1)(0)
    val env = Env()

    program.run(uid = d1)(env).nv shouldBe NValue(1)
    program.run(uid = d2)(env).nv shouldBe NValue(0)

  test("mux - 2"):
    val cond = NValue(true, Map(d1 -> true, d2 -> false))
    val trueNVal = NValue(1, Map(d1 -> 9))
    val program = mux(cond)(pure(trueNVal))(0)
    val env = Env()

    program.run(uid = d1)(env).nv shouldBe trueNVal
    program.run(uid = d2)(env).nv shouldBe NValue(0)

  test("alignment"):
    val program = nfold(0)(1)(_ + _) // Count neighbours

    // We'll use this vt for creating fake envs
    val vt = program.run(uid = d1)(Env())

    val zeroNeighboursEnv = Env()
    val oneNeighbourEnv = Env((d2 -> vt))
    val twoNeighboursEnv = Env(d2 -> vt, d3 -> vt)
    val threeNeighboursEnv = Env(d2 -> vt, d3 -> vt, d4 -> vt)
    program.run(uid = d1)(zeroNeighboursEnv).nv(d1) shouldBe 0
    program.run(uid = d1)(oneNeighbourEnv).nv(d1) shouldBe 1
    program.run(uid = d1)(twoNeighboursEnv).nv(d1) shouldBe 2
    program.run(uid = d1)(threeNeighboursEnv).nv(d1) shouldBe 3

  test("exchange"):
    val program = exchange(0)(n => retsend(n + 1))

    val d1vt0 = program.run(uid = d1)(Env())

    d1vt0.nv(d1) shouldBe 1
    d1vt0.nv(d2) shouldBe 1

    val d2vt0 = program.run(uid = d2)(Env(d1 -> d1vt0))

    d2vt0.nv(d1) shouldBe 2
    d2vt0.nv(d2) shouldBe 1

    val d1vt1 =
      program.run(uid = d1)(Env(d1 -> d1vt0, d2 -> d2vt0))

    d1vt1.nv(d1) shouldBe 2
    d1vt1.nv(d2) shouldBe 3

  test("nfold"):
    def countAlignedNeighbours: Aggregate[Int] =
      nfold(init = 0)(1)(_ + _)

    val d1vt0 = countAlignedNeighbours.run(uid = d1)(Env())
    val d2vt0 = countAlignedNeighbours.run(uid = d2)(Env())
    val d3vt0 = countAlignedNeighbours.run(uid = d3)(Env())

    d1vt0.nv(d1) shouldBe 0
    d2vt0.nv(d2) shouldBe 0
    d3vt0.nv(d3) shouldBe 0

    val d1vt1 = countAlignedNeighbours.run(uid = d1)(
      Env(d1 -> d1vt0, d2 -> d2vt0, d3 -> d3vt0)
    )
    val d2vt1 = countAlignedNeighbours.run(uid = d2)(
      Env(d1 -> d1vt0, d2 -> d2vt0, (d3 -> d3vt0))
    )
    val d3vt1 = countAlignedNeighbours.run(uid = d3)(
      Env(d1 -> d1vt0, d3 -> d3vt0)
    )

    d1vt1.nv(d1) shouldBe 2
    d2vt1.nv(d2) shouldBe 2
    d3vt1.nv(d3) shouldBe 1

  test("branch"):
    def sens = NValue(false, Map(d1 -> true, d2 -> false))
    val program = branch(sens)(0)(1)

    val d1vt0 = program.run(uid = d1)(Env())
    val d2vt0 = program.run(uid = d2)(Env())

    val env = Env(d1 -> d1vt0, d2 -> d2vt0)
    val d1vt1 = program.run(uid = d1)(env)
    val d2vt1 = program.run(uid = d2)(env)

    d1vt1.nv(d1) shouldBe 0
    d2vt1.nv(d2) shouldBe 1

  test("branch and alignment"):
    def countAlignedChild: Aggregate[Int] =
      nfold(init = 0)(1)(_ + _)
    def sens = NValue(false, Map(d1 -> true, d2 -> false))

    val program1 = branch(sens)(countAlignedChild)(countAlignedChild)

    val p1d1vt0 = program1.run(uid = d1)(Env())
    val p1d2vt0 = program1.run(uid = d2)(Env())

    val env1 = Env(d1 -> p1d1vt0, d2 -> p1d2vt0)

    val p1d1vt1 = program1.run(uid = d1)(env1)
    val p1d2vt1 = program1.run(uid = d2)(env1)

    p1d1vt1.nv(d1) shouldBe 0
    p1d2vt1.nv(d2) shouldBe 0

    val program2 = for
      count <- countAlignedChild
      res <- branch(sens)(pure(count))(count)
    yield res

    val p2d1vt0 = program2.run(uid = d1)(Env())
    val p2d2vt0 = program2.run(uid = d2)(Env())

    val env2 = Env(d1 -> p2d1vt0, d2 -> p2d2vt0)

    val p2d1vt1 = program2.run(uid = d1)(env2)
    val p2d2vt1 = program2.run(uid = d2)(env2)

    p2d1vt1.nv(d1) shouldBe 1
    p2d2vt1.nv(d2) shouldBe 1
