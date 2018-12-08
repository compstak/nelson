package nelson
package blueprint

import cats.syntax.semigroup._
import nelson.Manifest.EnvironmentVariable

trait BlueprintSyntax {
  import EnvValue._

  def envK(k: String, v: String): List[EnvironmentVariable] = EnvironmentVariable(k, v) :: Nil

  implicit class StringOps(val k: String) {
    def asValue[B: Write](b: B): Blueprint.CtxAtom = Write[B].make(k, b)
    def liftValue[B: Write](b: => Option[B]): Option[Blueprint.CtxAtom] = b.map(Write[B].make(k, _))
  }

  implicit class ElementOps(val ce0: Blueprint.CtxAtom) {
    def |*|(ce1: Blueprint.CtxAtom): MapValue = mkMap(ce0, ce1)
    def |/ : Map[String, EnvValue] = Map(ce0)
    def -| : MapValue = mkMap(ce0)
  }

  implicit class MapValueOps(val mv: MapValue) {
    def |*|(mv0: MapValue): MapValue = mv |+| mv0
    def |*|(ce: Blueprint.CtxAtom): MapValue = mv |+| mkMap(ce)
    def |*|(ce: Option[Blueprint.CtxAtom]): MapValue = ce.fold(mv)(mkMap(_) |+| mv)
    def |/ : Map[String, EnvValue] = mv.value
  }

  implicit def unbindMapValue(mv: MapValue): Map[String, EnvValue] = mv.value
}

object syntax extends BlueprintSyntax
