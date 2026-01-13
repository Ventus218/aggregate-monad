case class Tree[A](value: A, children: List[Tree[A]])
object Tree:
  extension [A](t: Tree[A])
    def flatMap[B](f: A => Tree[B]): Tree[B] =
      val NodeRoot = f(t.value)
      val newChildren = t.children.map(_.flatMap(f))
      Tree(NodeRoot.value, NodeRoot.children ++ newChildren)

  def pure[A](a: A): Tree[A] = Tree(a, Nil)

import org.scalacheck._
import org.scalacheck.Prop.forAll

object TreeMonadLaws extends Properties("TreeMonad"):

  given Arbitrary[Tree[Int]] = Arbitrary {
    def genTree(depth: Int): Gen[Tree[Int]] =
      for
        value <- Gen.choose(0, 100)
        numChildren <- Gen.choose(0, 8) // each node can have 0..3 children
        children <- if depth <= 0 then Gen.const(Nil)
                    else Gen.listOfN(numChildren, genTree(depth - 1))
      yield Tree(value, children)

    Gen.sized(size => genTree(size.min(5))) // allow depth up to 5
  }

  // More complex functions producing trees with multiple children
  val f: Int => Tree[Int] = x =>
    Tree(x + 1, List(Tree(x + 10, Nil), Tree(x + 20, List(Tree(3, Nil), Tree(5, Nil)))))

  val g: Int => Tree[Int] = x =>
    Tree(x * 2, List(Tree(x * 3, List(Tree(x + 2  , Nil))), Tree(x * 4, Nil), Tree(x * 5, List(Tree(x * 2 , Nil), Tree(x * 4 , Nil)))))

  // Pure
  def pure[A](a: A): Tree[A] = Tree(a, Nil)

  // Left identity: pure(a).flatMap(f) == f(a)
  property("left identity f") = forAll { (a: Int) =>
    pure(a).flatMap(f) == f(a)
  }
  property("left identity g") = forAll { (a: Int) =>
    pure(a).flatMap(g) == g(a)
  }

  // Right identity: m.flatMap(pure) == m
  property("right identity") = forAll { (m: Tree[Int]) =>
    m.flatMap(pure) == m
  }

  // Associativity: m.flatMap(f).flatMap(g) == m.flatMap(a => f(a).flatMap(g))
  property("associativity") = forAll { (m: Tree[Int]) =>
    m.flatMap(f).flatMap(g) == m.flatMap(a => f(a).flatMap(g))
  }
  // Associativity: m.flatMap(f).flatMap(g) == m.flatMap(a => f(a).flatMap(g))
  property("associativity 2") = forAll { (m: Tree[Int]) =>
    m.flatMap(g).flatMap(f) == m.flatMap(a => g(a).flatMap(f))
  }
