package main

import java.sql.Timestamp
import java.time.Instant

import api.Prob
import Prob._

case class User(id: String, ip: String, firstInteraction: Timestamp)

case class Clic(userId: String, ip: String, pageCategory: String, clicType: String, ts: Timestamp) {
  def addRandomlyMillis: Clic = this.copy(ts = new Timestamp(ts.getTime + Uniform(3 * 60000, 15 * 60000).get.toLong))
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

  def probPageCategory = DiscreteUniform(Seq("Health", "Sport", "Technology", "Science", "Business", "Entertainment"))

  def probType(category: String): Prob[String] =

    if (Seq("Health", "Technology", "Science", "Business") contains category)
      product(Bernoulli(0.9), Bernoulli(0.5)) flatMap {
        case (true, _)     ⇒ DiscreteUniform(Seq("mute", "stop"))
        case (false, true) ⇒ unit("start")
        case _             ⇒ DiscreteUniform(Seq("mute", "start", "stop"))
      }
    else DiscreteUniform(Seq("mute", "start", "stop"))

  def clicProb(user: User): Prob[Clic] = for {
    pageCat ← probPageCategory
    clicType ← probType(pageCat)
  } yield Clic(user.id, user.ip, pageCat, clicType, user.firstInteraction)

  val nbInteractionProb: Prob[Int] = flatten(Bernoulli(0.3) to (DiscreteUniform(2 to 3), unit(1)))

  val clicsProb: Prob[Seq[Clic]] = map2(userProb, nbInteractionProb) {
    case (user: User, nbInt: Int) ⇒
      clicProb(user)
        .sample(nbInt)
        .map(_.addRandomlyMillis).sortWith { case (c1, c2) ⇒ c1.ts before c2.ts }
  }

  clicsProb.sample(10).flatten.foreach(println)

  /*
Clic(68d910a4-6719-489d-b537-695cb7a280a1,146.116.103.120,Science,stop,2019-06-18 00:05:53.808)
Clic(35874c5e-1819-4229-b2d9-255f19cd9951,139.221.232.220,Business,stop,2019-06-17 11:33:34.082)
Clic(290566e6-9fa7-4d04-8269-34a13c80980f,175.147.158.224,Health,stop,2019-06-18 14:16:15.708)
Clic(375692f2-e606-4729-ba41-ff320ab80b7a,138.169.215.146,Business,stop,2019-06-21 19:02:45.642)
Clic(a8cdf77b-ef32-4348-9073-4567621f5131,140.122.148.247,Sport,start,2019-06-16 02:36:38.694)
Clic(c6883304-7293-469e-b0c5-f47714800bde,111.215.192.138,Business,mute,2019-06-17 02:00:34.251)
Clic(c6883304-7293-469e-b0c5-f47714800bde,111.215.192.138,Technology,mute,2019-06-17 02:08:51.915)
Clic(4a5acbbb-f528-4b88-ad4d-161889746a08,230.208.169.246,Entertainment,stop,2019-06-16 22:56:34.405)
Clic(1f636b72-e9ac-4f1b-ab72-d4d51e7361af,156.113.125.241,Science,mute,2019-06-19 18:04:10.986)
Clic(1f636b72-e9ac-4f1b-ab72-d4d51e7361af,156.113.125.241,Business,stop,2019-06-19 18:07:43.216)
Clic(1f636b72-e9ac-4f1b-ab72-d4d51e7361af,156.113.125.241,Technology,stop,2019-06-19 18:10:59.688)
Clic(cdca4b19-d8b2-4132-9bda-e1e33e7c69d6,127.146.172.202,Technology,stop,2019-06-15 04:26:12.985)
Clic(cdca4b19-d8b2-4132-9bda-e1e33e7c69d6,127.146.172.202,Technology,mute,2019-06-15 04:27:58.966)
Clic(66e9184d-b536-468c-934a-33b24a779404,169.233.155.141,Technology,mute,2019-06-20 08:13:35.569)
   */

}
