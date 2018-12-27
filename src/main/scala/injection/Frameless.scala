package injection

import model.{ EventType, Picture, Video }

object Frameless {

  case class EventTypeInj(win: Double, duration: Option[Long])

  import frameless._

  implicit val eventTypeInjection: Injection[EventType, EventTypeInj] = Injection[EventType, EventTypeInj](

    {
      case Picture(win)    ⇒ EventTypeInj(win, None)
      case Video(win, dur) ⇒ EventTypeInj(win, Some(dur))
    },

    {
      case EventTypeInj(win, Some(dur)) ⇒ Video(win, dur)
      case EventTypeInj(win, None)      ⇒ Picture(win)
    }

  )

}
