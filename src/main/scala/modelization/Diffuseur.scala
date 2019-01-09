package modelization

import java.sql.Timestamp
import java.time.Instant
import java.util.UUID

import api.Prob
import api.Prob.{ DiscreteUniform, Poisson, Uuid }

sealed trait DiffuseurType
case object Sport extends DiffuseurType
case object Luxe extends DiffuseurType

sealed trait DiffuseurName {
  def diffuseurType: DiffuseurType
}

case object Adidas extends DiffuseurName {
  override def diffuseurType: DiffuseurType = Sport
}
case object Nike extends DiffuseurName {
  override def diffuseurType: DiffuseurType = Sport
}

case object Dior extends DiffuseurName {
  override def diffuseurType: DiffuseurType = Luxe
}

case class Taille(
    longueur: Int,
    largeur: Int)

sealed trait Type {
  def id: UUID
}

case class Video(
    id: UUID,
    win: Double,
    duree: Double
) extends Type

case class Picture(
    id: UUID,
    win: Double,
    taille: Taille
) extends Type

case class Campagne(
    id: UUID,
    debut: Timestamp,
    fin: Timestamp,
    budget: Double
)

case class Diffusion(
    ts: Timestamp,
    `type`: Type,
    isClicked: Boolean,
    isViewed: Boolean
)

case class Diffuseur(
    name: DiffuseurName,
    diffusions: Seq[Diffusion] // loi de Poisson sur les clics
)

/**
 *
 */
object Diffuseur {

  def genererEvenement() = {

    val probUUID: Prob[UUID] = Uuid.map(UUID.fromString)

    val probDiffuseurName: Prob[DiffuseurName] = DiscreteUniform(
      Seq.fill(6)(Adidas) ++
        Seq.fill(5)(Nike) ++
        Seq.fill(2)(Dior)
    )

    def nbDiffusion(name: DiffuseurName) = name match {
      case Nike | Adidas ⇒ {

        val probTS: Prob[Timestamp] = {
          val nowMs = Instant.now().getEpochSecond * 1000
          val fromNowToOneDay = (1L to 24 * 3600).map(i ⇒ new Timestamp(i * 1000 + nowMs))
          DiscreteUniform(fromNowToOneDay)
        }
      }
    }

  }
}
