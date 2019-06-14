package main

import java.sql.Timestamp
import java.time.Instant

import api.Prob
import Prob._

case class User(id: String, ip: String, firstInteraction: Timestamp)

case class Clic(userId: String, ip: String, pageCategory: String, clicType: String, ts: Timestamp) {
  def addRandomlyMillis: Clic = this.copy(ts = new Timestamp(ts.getTime + Uniform(0, 10000).get.toLong))
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
User(5c082cc9-2d96-44a9-9250-299a79fbadfd,195.125.203.151,2019-06-15 14:19:18.0)
User(273b5b0b-8f76-42a0-8f5f-d547681652c9,103.223.225.188,2019-06-18 01:23:33.0)
User(303e6715-70f5-42ce-b5a3-35be6b7c8f4d,226.180.149.137,2019-06-20 07:05:27.0)
User(63f2a2a4-e36e-4597-885f-7278f1bf25a6,156.100.182.215,2019-06-17 20:56:07.0)
User(36eda7d4-dca6-4951-b7db-a11dd9fde3b0,158.206.138.201,2019-06-16 17:54:02.0)
User(206cf30c-7bf3-42be-9066-ed1601325d24,165.148.240.185,2019-06-15 12:34:48.0)
User(895061f0-a86d-44c6-be39-41a46b8ce83c,184.255.196.104,2019-06-17 07:25:12.0)
User(a8a91f55-cb4e-4fe3-83a4-a65d117827f2,110.133.149.112,2019-06-19 04:45:01.0)
User(5e42f77b-b59d-4d50-9083-30a115ba109f,143.131.115.182,2019-06-20 23:18:29.0)
User(948a468f-1d45-49cb-af0d-7f8cb8d7b085,122.191.129.240,2019-06-17 14:00:54.0)
   */

  val probCategory = DiscreteUniform(Seq("Health", "Sport", "Technology", "Science", "Business", "Entertainment"))

  def probType(category: String): Prob[String] =

    if (Seq("Health", "Technology", "Science", "Business") contains category) product(Bernoulli(0.9), Bernoulli(0.5)) flatMap {
      case (true, _)     ⇒ DiscreteUniform(Seq("mute", "stop"))
      case (false, true) ⇒ unit("start")
      case _             ⇒ DiscreteUniform(Seq("mute", "start", "stop"))
    }
    else DiscreteUniform(Seq("mute", "start", "stop"))

  def clicProb(user: User): Prob[Clic] = for {
    cat ← probCategory
    pageType ← probType(cat)
  } yield Clic(user.id, user.ip, cat, pageType, user.firstInteraction)

  val nbInteractionProb: Prob[Int] = flatten(Bernoulli(0.3) to (DiscreteUniform(2 to 3), unit(1)))

  val clicsProb: Prob[Seq[Clic]] = map2(userProb, nbInteractionProb) {
    case (user, nbInt) ⇒
      clicProb(user)
        .sample(nbInt)
        .map(_.addRandomlyMillis).sortWith { case (c1, c2) ⇒ c1.ts.before(c2.ts) }
  }

  clicsProb.sample(10).flatten.foreach(println)

  /*
Clic(8d407101-fb7f-4f18-a108-d9c75f16ffbb,211.252.227.104,Business,stop,2019-06-15 17:09:24.012)
Clic(8d407101-fb7f-4f18-a108-d9c75f16ffbb,211.252.227.104,Science,stop,2019-06-15 17:09:33.7)
Clic(b8b3e627-2604-40dd-96a9-e6a1e3d2c311,228.151.152.160,Health,stop,2019-06-20 02:45:11.146)
Clic(b8b3e627-2604-40dd-96a9-e6a1e3d2c311,228.151.152.160,Health,mute,2019-06-20 02:45:11.925)
Clic(b8b3e627-2604-40dd-96a9-e6a1e3d2c311,228.151.152.160,Business,stop,2019-06-20 02:45:14.494)
Clic(3249a672-0593-4db9-a838-2d5e28b8b6e0,152.225.204.160,Science,stop,2019-06-18 06:08:13.888)
Clic(ec7deb5f-2919-4a3c-912d-111624425e80,197.245.176.180,Health,mute,2019-06-21 04:55:24.367)
Clic(a8b06959-aa94-4248-bd4d-474208ed8cba,228.224.118.250,Entertainment,stop,2019-06-15 10:49:19.856)
Clic(a8b06959-aa94-4248-bd4d-474208ed8cba,228.224.118.250,Business,mute,2019-06-15 10:49:22.695)
Clic(a8b06959-aa94-4248-bd4d-474208ed8cba,228.224.118.250,Entertainment,stop,2019-06-15 10:49:26.494)
Clic(b21291ef-e7e7-4705-b2e6-22d4b3478374,208.114.140.105,Technology,stop,2019-06-19 11:57:31.069)
Clic(d69b7916-da49-4443-a1fe-99802a148d89,109.242.184.190,Business,mute,2019-06-16 12:32:16.955)
Clic(e6cbf703-3275-4116-a80a-d81772a34925,144.117.241.229,Science,stop,2019-06-14 20:21:22.98)
Clic(e6cbf703-3275-4116-a80a-d81772a34925,144.117.241.229,Health,stop,2019-06-14 20:21:25.792)
Clic(e6cbf703-3275-4116-a80a-d81772a34925,144.117.241.229,Health,stop,2019-06-14 20:21:27.092)
Clic(e5122de4-679e-4442-a57b-7eb0ac216e00,112.184.182.148,Health,mute,2019-06-19 04:35:41.613)
Clic(e5122de4-679e-4442-a57b-7eb0ac216e00,112.184.182.148,Health,mute,2019-06-19 04:35:42.972)
   */

}
