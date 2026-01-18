package aggregate

import org.scalacheck._
import org.scalacheck.Prop.forAll
import NValues.*
import AggregateAPI.Device

object NValueMonadLaws extends Properties("NValueMonadLaws"):

  def genNValue: Gen[NValue[Int]] =
    for
      value <- Gen.choose(0, 100)
      nDevices <- Gen.choose(0, 20)
      overrides <- Gen.listOfN(nDevices, Gen.choose(0, 100))
    yield NValue(
      value,
      List.tabulate(nDevices)(Device.fromInt).zip(overrides).toMap
    )

  given Arbitrary[NValue[Int]] = Arbitrary(genNValue)

  val f: Int => NValue[Int] = x =>
    NValue(x + 1, Map(Device.fromInt(12) -> (x - 1)))

  val g: Int => NValue[Int] = x =>
    NValue(x * 4, Map(Device.fromInt(11) -> (x + 8)))

  // Pure
  def pure[A](a: A): NValue[A] = NValue(a)

  // Left identity: pure(a).flatMap(f) == f(a)
  property("left identity generic function") = {
    forAll: (a: Int, f: Int => NValue[Int]) =>
      pure(a).flatMap(f) == f(a)
  }
  property("left identity f") = {
    forAll: (a: Int) =>
      pure(a).flatMap(f) == f(a)
  }
  property("left identity g") = {
    forAll: (a: Int) =>
      pure(a).flatMap(g) == g(a)
  }

  // Right identity: nv.flatMap(pure) == m
  property("right identity") = {
    forAll: (nv: NValue[Int]) =>
      nv.flatMap(pure) == nv
  }

  // Associativity: nv.flatMap(f).flatMap(g) == nv.flatMap(a => f(a).flatMap(g))
  property("associativity generic functions") = {
    forAll: (nv: NValue[Int], f: Int => NValue[Int], g: Int => NValue[Int]) =>
      nv.flatMap(f).flatMap(g) == nv.flatMap(a => f(a).flatMap(g))
  }
  property("associativity f") = {
    forAll: (nv: NValue[Int]) =>
      nv.flatMap(f).flatMap(g) == nv.flatMap(a => f(a).flatMap(g))
  }
  // Associativity: nv.flatMap(f).flatMap(g) == nv.flatMap(a => f(a).flatMap(g))
  property("associativity g") = {
    forAll: (nv: NValue[Int]) =>
      nv.flatMap(g).flatMap(f) == nv.flatMap(a => g(a).flatMap(f))
  }
