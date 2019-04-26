package model

import java.sql.Timestamp
import java.time.Instant

import api.Prob
import api.Prob.{ Bernoulli, DiscreteUniform, Uuid }
import model.EventType.generateEventType

case class Event(
    id: String,
    ts: Long,
    `type`: EventType)

object Event {

  /**
   * Between 8pm and 3am: videos have more chance to be generated than pictures
   * @param ts
   * @return
   */
  def evtP(ts: Timestamp): Prob[EventType] = {
    if (ts.getHours <= 3 || ts.getHours >= 20) Bernoulli(0.3).flatMap(generateEventType)
    else Bernoulli(0.8).flatMap(generateEventType)
  }

  /**
   * Event generator
   * @return
   */
  def probEvent(from: Instant = Instant.now()): Prob[Event] = {

    val tsP: Prob[Timestamp] = Prob(() ⇒ {
      val seq: Seq[Timestamp] = (0 to 24 * 3600).map(s ⇒ new Timestamp((from.getEpochSecond + s) * 1000))
      DiscreteUniform(seq).get
    }
    )

    for {
      ip ← Uuid
      ts ← tsP
      evt ← evtP(ts)
    } yield Event(ip, ts.getTime, evt)
  }
}