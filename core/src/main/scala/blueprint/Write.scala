package nelson
package blueprint

import cats.data.NonEmptyList
import simulacrum.typeclass

@typeclass
trait Write[A] {
  def apply(k: String, a: A): Blueprint.CtxAtom
}

object Write {

  def construct[A](fa: A => EnvValue): Write[A] =
    new Write[A] {
      def apply(k: String, a: A): Blueprint.CtxAtom = (k, fa(a))
    }

  implicit val stringWrite: Write[String] =
    construct(EnvValue.StringValue)

  implicit val listWrite: Write[List[EnvValue]] =
    construct(EnvValue.ListValue)

  implicit val nelWrite: Write[NonEmptyList[EnvValue]] =
    construct(nel => EnvValue.ListValue(nel.toList))

  implicit val mapWrite: Write[Map[String, EnvValue]] =
    construct(EnvValue.MapValue)
}