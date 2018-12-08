package nelson.blueprint

import cats.{Eq, Monoid, Semigroup}
import cats.implicits._

import org.fusesource.scalate.{TemplateEngine, TemplateSource, Template => STemplate}

final class Template private(private val template: STemplate, override val toString: String) {
  def render(env: Map[String, EnvValue]): String =
    // We get away with empty string here because we turn off
    // caching in the template engine
    Template.engine.layout("", template, Template.envToMap(env.toList, Map.empty))
}

object Template {
  /** Create a template from a raw string. */
  def load(id: String, templateString: String): Template = {
    // NOTE: If TemplateEngine has caching on, it will use the id
    // as the cache key. We also need to tack on '.mustache' here
    // since Scalate uses the assumed extension to determine
    // which backend to use :fire:
    val source = TemplateSource.fromText(s"${id}.mustache", templateString)
    val template = engine.load(source)
    new Template(template, templateString)
  }

  private val engine = {
    val te = new TemplateEngine()
    te.allowReload  = false
    te.allowCaching = false
    te
  }

  implicit val nelsonBlueprintTemplateInstances: Eq[Template] = new Eq[Template] {
    def eqv(x: Template, y: Template): Boolean = x.toString == y.toString
  }

  @annotation.tailrec
  private def envToMap(envs: List[(String, EnvValue)], acc: Map[String, Any]): Map[String, Any] = envs match {
    case Nil => acc
    case (key, value) :: tail => envToMap(tail, acc + ((key, extractValue(value))))
  }

  import EnvValue._
  private def extractValue(env: EnvValue): Any = env match { // YOLO
    case StringValue(v) => v
    case ListValue(v) => v.map(extractValue)
    case MapValue(v) => v.map { case (k, v) => (k, extractValue(v)) }
  }
}

sealed abstract class EnvValue extends Product with Serializable

object EnvValue {
  final case class StringValue(value: String) extends EnvValue
  final case class ListValue(value: List[EnvValue]) extends EnvValue
  final case class MapValue(value: Map[String, EnvValue]) extends EnvValue

  private[this] object unapply {
    val (s, l, m) = (StringValue, ListValue, MapValue)
  }

  def Empty: MapValue = mapValueMonoid.empty

  def mkString(value: String) = StringValue(value)

  def mkList(values: EnvValue*) = ListValue(values.toList)

  def mkMap(values: (String, EnvValue)*) = MapValue(values.toMap)

  implicit val envValueEq: Eq[EnvValue] =
    new Eq[EnvValue] {
      import unapply._
      def eqv(x: EnvValue, y: EnvValue): Boolean =
        (x, y) match {
          case (s(sx), s(sy)) => sx === sy
          case (l(lx), l(ly)) => if (lx.size =!= ly.size) false
                                 else lx.forall(ly.contains) // todo improve equality
          case (m(mx), m(my)) => if (mx.size =!= my.size) false
                                 else mx.forall(x => my(x._1) == x._2) // todo improve equality
          case _              => false
        }
    }

  implicit val stringValueSemigroup: Semigroup[StringValue] =
    new Semigroup[StringValue] {
      def combine(x: StringValue,
                  y: StringValue): StringValue = y
    }

  implicit val listValueSemigroup: Semigroup[ListValue] =
    new Semigroup[ListValue] {
      def combine(x: ListValue,
                  y: ListValue): ListValue =
        ListValue(x.value |+| y.value)
    }

  implicit val mapValueMonoid: Monoid[MapValue] =
    new Monoid[MapValue] {
      def empty: MapValue = MapValue(Map.empty)
      def combine(x: MapValue,
                  y: MapValue): MapValue =
        MapValue(x.value |+| y.value)
    }

  implicit val envValueSemigroup: Semigroup[EnvValue] =
    new Semigroup[EnvValue] {
      import unapply._
      def combine(x: EnvValue, y: EnvValue): EnvValue =
        (x, y) match {
          case (x@l(_), y@l(_)) => x |+| y
          case (x@m(_), y@m(_)) => x |+| y
          case _                => y
        }
    }
}
