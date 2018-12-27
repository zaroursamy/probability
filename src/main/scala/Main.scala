import api.Prob
import api.Prob.Uniform
import config.SparkConfig.ss
import frameless._
import frameless.syntax._
import model.{ Event, Video }

object Main extends App {
  val N = 100

  Uniform(1, 10)
    .map(u ⇒ math.round(u).toInt)
    .filter(_ % 2 == 0).density(identity, N)
    .toSeq
    .sortWith(_._1 < _._1)
    .foreach(println)

  System.exit(0)

  val adEventP: Prob[Event] = Event.probEvent()

  import injection.Frameless._
  eventTypeInjection

  val eventDS: TypedDataset[Event] = TypedDataset.create(adEventP.sample(N)).persist()

  eventDS.show().run()
  eventDS.printSchema()

  ss.time(
    println(s"[Dataset] Prob(video) = ${eventDS.toDF.filter("type.duration is not null").count().toDouble / N}")
  )

  ss.time(
    println(s"[Prob] Prob(video) = ${adEventP.prob(_.`type`.isInstanceOf[Video], N)}")
  )

}
