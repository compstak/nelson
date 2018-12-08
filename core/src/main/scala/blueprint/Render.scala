package nelson
package blueprint

import nelson.Datacenter.StackName
import nelson.Manifest._
import nelson.docker.Docker.Image

import cats.implicits._

import simulacrum.typeclass

@typeclass
trait Render[A] {
  def from(a: A): Blueprint.Ctx
}

object Render {
  import Blueprint._
  import EnvValue._

  import keys._
  import syntax._

  private[this] type R[A] = Render[A]

  def makeDefaultEnv(i: Image, dc: Datacenter, ns: NamespaceName, u: UnitDef, v: Version, p: Plan, hash: String): Blueprint.Ctx =
    makeEnv(Context(i, dc, ns, u, v, p, hash))

  def makeEnv[A0](a0: A0)(implicit CR0: R[A0]): Blueprint.Ctx =
    CR0.from(a0)

  def makeEnv[A0, A1](a0: A0, a1: A1)(implicit CR0: R[A0], CR1: R[A1]): Blueprint.Ctx =
    CR0.from(a0) |+| CR1.from(a1)

  final def PassThrough[A]: Render[A] =
    new Render[A] {
      def from(a: A): Blueprint.Ctx = Map.empty
    }

  /**
    * The default context used when rendering a blueprint.
    *
    *
    */
  final case class Context(img: Image, dc: Datacenter, ns: NamespaceName, unit: UnitDef, v: Version, p: Plan, hash: String)

  implicit val baseContextRenderer: Render[Context] =
    new Render[Context] {
      def from(base: Context): Map[String, EnvValue] = {
        import base._
        import p.{environment => env}

        val sn = StackName(unit.name, v, hash)

        /**
          *
          */
        (stackName  asValue sn.toString) |*|
        (namespace  asValue ns.root.asString) |*|
        (unitName   asValue sn.serviceType) |*|
        (version    asValue sn.version.toString) |*|
        (image      asValue img.toString) |*|
        (datacenter asValue dc.name) |*|
        /**
          *
          */
        (desiredInstances liftValue env.desiredInstances.map(_.toString)) |*|
        (schedule         liftValue maybeSchedule(unit, p)) |*|
        (retries          liftValue env.retries.map(_.toString)) |*|
        /**
          *
          */
        ports        liftValue unit.ports.map(mkPorts(_).|/) |*|
        emptyVolumes liftValue env.volumes.map(mkVolume).headOption.map(_.|/) |*|
        requestOrLimit(env.cpu)(
          cpuLimit,
          cpuRequest) |*|
        requestOrLimit(env.memory)(
          memoryLimit,
          memoryRequest) |*|
        /**
          *
          */
        headOpt(env.healthChecks)(hc => mkHealthCheck(hc)) |*|
        /**
          *
          */
        (envvars asValue loadEnvironment(
          p.environment.bindings,
          envK("NELSON_DNS_ROOT", dc.domain.name) |+|
          envK("NELSON_DATACENTER", dc.name)      |+|
          envK("NELSON_ENV", ns.root.asString)    |+|
          envK("NELSON_NAMESPACE", ns.asString)   |+|
          envK("NELSON_PLAN", p.name)             |+|
          envK("NELSON_STACKNAME", sn.toString)). |/).-|
      }
    }

  def headOpt[A](as: List[A])(fa: A => MapValue): MapValue =
    as.headOption.fold(EnvValue.Empty)(fa)

  def loadEnvironment(evs: List[Manifest.EnvironmentVariable]*): MapValue =
    (envvarsList asValue evs.toList.foldMap(_.map(mkEnvironmentVariable).widen[EnvValue]))
      .-|

  def maybeSchedule(unit: UnitDef, p: Plan): Option[String] = Manifest.getSchedule(unit, p) >>= (_.toCron())

  def mkEnvironmentVariable(ev: Manifest.EnvironmentVariable): MapValue =
    (envvarName  asValue ev.name) |*|
    (envvarValue asValue ev.value)

  def mkHealthCheck(hc: Manifest.HealthCheck): MapValue =
    (healthCheck asValue (
      (healthCheckPath     asValue hc.path.getOrElse("/")         )|*|
      (healthCheckPort     asValue hc.portRef.toString            )|*|
      (healthCheckInterval asValue hc.interval.toSeconds.toString )|*|
      (healthCheckTimeout  asValue hc.timeout.toSeconds.toString)).|/).-|

  def mkPort(port: Manifest.Port): MapValue =
    (portName   asValue port.ref) |*|
    (portNumber asValue port.port.toString)

  def mkPorts(ports: Manifest.Ports): MapValue =
    (portsList asValue ports.nel.map(mkPort).widen[EnvValue]).-|

  def mkVolume(vr: Manifest.Volume): MapValue =
    (emptyVolumeMountName asValue vr.name)               |*|
    (emptyVolumeMountPath asValue vr.mountPath.toString) |*|
    (emptyVolumeMountSize asValue vr.size.toString)

  def requestOrLimit(rs: Manifest.ResourceSpec)(lk: String, rk: String): MapValue =
    rs.fold(
      unspecified = EnvValue.Empty,
      limitOnly   =      l => (lk asValue l.toString).-|,
      bounded     = (r, l) => (rk asValue r.toString) |*|
                              (lk asValue l.toString))
}
