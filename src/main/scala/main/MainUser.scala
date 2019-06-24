package main

import java.sql.Timestamp
import java.time.Instant

import api.Prob
import Prob._

case class User(id: String, ip: String, firstInteraction: Timestamp)

case class Event(userId: String, ip: String, pageCategory: String, clicType: String, ts: Timestamp) {
  def addRandomTs: Event = this.copy(
    ts = new Timestamp(ts.getTime + Uniform(3 * 60000, 15 * 60000).get.toLong)
  )
}

case class AvroEvent(userId: String, ip: String, pageCategory: String, clicType: String, ts: Long)
object AvroEvent {
  def fromEvent(clic: Event) = AvroEvent(
    userId = clic.userId,
    ip = clic.ip,
    pageCategory = clic.pageCategory,
    clicType = clic.clicType,
    ts = clic.ts.getTime
  )
}

object MainUser extends App {
  lazy val now = Instant.now()

  val idProb: Prob[String] = Uuid

  val ipProb: Prob[String] = {

    val ipRange = DiscreteUniform(100 to 255)

    for {
      i1 ← ipRange
      i2 ← ipRange
      i3 ← ipRange
      i4 ← ipRange
    } yield s"$i1.$i2.$i3.$i4"
  }

  def tsProb(from: Instant, nbDay: Int = 7): Prob[Timestamp] =
    DiscreteUniform(0 to 24 * 3600 * nbDay) map { s ⇒ new Timestamp((s + from.getEpochSecond) * 1000) }

  val userProb: Prob[User] = for {
    id ← idProb
    ip ← ipProb
    ts ← tsProb(now)
  } yield User(id, ip, ts)

  userProb.sample(10).foreach(println)
  /*
User(8eedb8f9-d908-429b-80aa-49a55b7739c7,242.166.207.216,2019-06-19 23:41:10.0)
User(7fd84613-956d-4478-b283-ff4ecf48a638,222.194.141.128,2019-06-18 03:26:02.0)
User(3fce086b-045d-43a4-a750-7749187619d2,131.202.223.119,2019-06-17 19:38:28.0)
User(49801d5c-5b91-4d08-ae8b-ff7ad57d6d10,110.163.106.129,2019-06-17 16:45:31.0)
User(c5d88dc8-d9c4-4cb3-924e-8a16e4e18ac3,138.170.148.241,2019-06-19 23:34:40.0)
User(335d9fbb-5327-4696-826d-502e33f506cf,156.209.107.124,2019-06-19 20:05:48.0)
User(b8bd7777-b818-4126-9244-0308deeb6433,150.159.220.132,2019-06-21 14:05:10.0)
User(38a142f8-9198-4816-a390-1714dd1fa05a,240.231.143.223,2019-06-15 10:39:49.0)
User(db8edccb-cc0b-44b3-a423-56d1d9caded8,223.163.192.208,2019-06-21 10:12:24.0)
User(dfc5a15c-fb2b-4b1a-b867-8551002cfabd,231.133.154.117,2019-06-18 04:34:46.0)
   */

  def probPageCategory: Prob[String] = DiscreteUniform(Seq("Health", "Sport", "Technology", "Science", "Business", "Entertainment"))

  def probEventType(category: String): Prob[String] =
    if (Seq("Health", "Technology", "Science", "Business") contains category)
      flatten(Bernoulli(0.9) to (DiscreteUniform(Seq("mute", "stop")), unit("start")))
    else DiscreteUniform(Seq("mute", "start", "stop"))

  def eventProb(user: User): Prob[Event] = for {
    pageCat ← probPageCategory
    clicType ← probEventType(pageCat)
  } yield Event(user.id, user.ip, pageCat, clicType, user.firstInteraction)

  val nbInteractionProb: Prob[Int] = flatten(Bernoulli(0.3) to (DiscreteUniform(2 to 3), unit(1)))

  val eventProb: Prob[Seq[Event]] = map2(userProb, nbInteractionProb) {
    case (user: User, nbInt: Int) ⇒
      eventProb(user)
        .sample(nbInt)
        .map(_.addRandomTs).sortWith { case (c1, c2) ⇒ c1.ts before c2.ts }
  }

  eventProb.sample(10).flatten.foreach(println)
  /*
Event(a717b42b-744f-42bd-a71d-43f0604bf81d,217.232.233.129,Sport,start,2019-06-25 21:49:35.46)
Event(a717b42b-744f-42bd-a71d-43f0604bf81d,217.232.233.129,Business,stop,2019-06-25 21:50:54.617)
Event(a717b42b-744f-42bd-a71d-43f0604bf81d,217.232.233.129,Business,stop,2019-06-25 21:54:49.105)
Event(33613da6-aef0-4131-8cd1-070e82f6a1bf,176.196.237.231,Health,mute,2019-06-25 05:35:41.113)
Event(33613da6-aef0-4131-8cd1-070e82f6a1bf,176.196.237.231,Health,stop,2019-06-25 05:39:37.432)
Event(71cd54bc-6db6-4014-8f03-48a618250f28,216.190.194.123,Science,stop,2019-06-26 01:53:14.783)
Event(71cd54bc-6db6-4014-8f03-48a618250f28,216.190.194.123,Technology,mute,2019-06-26 01:57:14.632)
Event(053417c2-6920-4887-b4c4-e21335b19003,177.177.172.136,Business,mute,2019-06-27 16:09:02.358)
Event(053417c2-6920-4887-b4c4-e21335b19003,177.177.172.136,Sport,mute,2019-06-27 16:14:52.372)
Event(f7eb18b1-5593-417c-b8a6-f44210c45ddc,176.149.200.207,Business,mute,2019-06-25 07:44:04.271)
Event(72720f43-67b0-4493-8516-b21dff214d4b,132.186.131.166,Technology,stop,2019-06-26 10:02:23.993)
Event(276013d1-c1c1-4dea-a74d-fed8fc4970b9,102.201.168.178,Sport,mute,2019-06-24 06:51:33.231)
Event(c4a5da3d-36df-463c-bdb5-9cd7564e076b,106.191.250.144,Health,stop,2019-06-25 00:31:01.658)
Event(e2c7ae83-83fd-4a49-b082-433f984c70ee,185.128.223.111,Technology,mute,2019-06-24 23:02:33.552)
Event(e2c7ae83-83fd-4a49-b082-433f984c70ee,185.128.223.111,Technology,stop,2019-06-24 23:03:16.946)
Event(19314980-aef0-4783-ae57-e552ad996109,147.228.214.213,Health,start,2019-06-25 23:46:11.782)
   */

}
