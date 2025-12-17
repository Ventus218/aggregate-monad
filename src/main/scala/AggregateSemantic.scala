import NValues.*
import scala.language.implicitConversions
import AggregateGrammar.*

type Device = Int
case class Env(nvalues: Map[Device, ValueTree[Any]], self: Device)

enum ValueTree[A]:
  // cond = None -> root
  case Branch(cond: Option[Boolean], children: Seq[ValueTree[A]])
  case Exchange(send: NValue[A])

object ValueTree:
  def empty[A]: ValueTree[A] = ValueTree.Branch(None, Seq())

import cats.data.State
import cats.data.ReaderT
type ValueTreeState[A] = State[ValueTree[Any], A]
type Round[A] = ReaderT[ValueTreeState, Env, A]
import cats.~>
def compiler: AggregateGrammar ~> Round =
  new (AggregateGrammar ~> Round):
    def apply[A](fa: AggregateGrammar[A]): Round[A] =
      fa match
        case Exchange(init, body) =>
          for
            env <- ReaderT.ask[ValueTreeState, Env]
            // TODO: align and then give as input to the body the right NValues
            (ret, send) = body
              .asInstanceOf[NValue[Any] => (A, NValue[Any])](env.nvalues)
            _ <- ReaderT.liftF(
              State.modify[ValueTree[Any]](curr =>
                curr match
                  case ValueTree.Branch(cond, vts) =>
                    ValueTree.Branch(cond, vts :+ ValueTree.Exchange(send))
                  case ValueTree.Exchange(_) =>
                    throw new IllegalStateException()
              )
            )
          yield (ret)
        case Branch(cond, the, els) =>
          for
            env <- ReaderT.ask[ValueTreeState, Env]
            branch = if cond then the else els
            (vt, ret) = branch
              .foldMap(compiler)
              .run(env)
              .run(ValueTree.Branch(Some(cond), Seq()))
              .value
            _ <- ReaderT.liftF(
              State.modify[ValueTree[Any]](curr =>
                curr match
                  case ValueTree.Branch(c, vts) =>
                    ValueTree.Branch(c, vts :+ vt)
                  case ValueTree.Exchange(_) =>
                    throw new IllegalStateException()
              )
            )
          yield (ret)

@main
def p: Unit =
  val program =
    for
      _ <- exchange(0, _ => retsend(0))
      _ <- exchange(0, _ => retsend(0))
      res <- branch(
        true,
        for
          _ <- exchange(0, _ => retsend(0))
          _ <- branch(
            true,
            for
              _ <- exchange(0, _ => retsend(0))
              _ <- exchange(0, _ => retsend(0))
            yield (),
            exchange(0, _ => retsend(0))
          )
        yield (),
        exchange(0, _ => retsend(0))
      )
    yield (res)
  val round = program.foldMap(compiler)
  val env = Env(Map(), 3)
  println(round(env).run(ValueTree.empty).value)

  val program2 =
    for
      _ <- exchange(0, _ => retsend(0))
      _ <- exchange(0, _ => retsend(0))
      res <- branch(
        false,
        for
          _ <- exchange(0, _ => retsend(0))
          _ <- branch(
            true,
            for
              _ <- exchange(0, _ => retsend(0))
              _ <- exchange(0, _ => retsend(0))
            yield (),
            exchange(0, _ => retsend(0))
          )
        yield (),
        exchange(0, _ => retsend(0))
      )
    yield (res)
  val round2 = program2.foldMap(compiler)
  println(round2(env).run(ValueTree.empty).value)

  // TODO: stackoverflow
  // should convert then and else to by name parameters
  def f(n: Int): Aggregate[Int] =
    branch(n >= 0, f(n - 1), exchange(0, _ => retsend(0)))
  val program3 = f(3)
  val round3 = program3.foldMap(compiler)
  println(round3(env).run(ValueTree.empty).value)
