package main

import api.Prob
import api.Prob.Ip
import config.SparkConfig.ss
import frameless.TypedDataset
import model.{Event, Video}
import injection.Frameless._

object MainGenerateDataset extends App {
  eventTypeInjection

  val N = 100

  Ip.sample(10).foreach(println)

  System.exit(0)

  val adEventP: Prob[Event] = Event.probEvent()

  val eventDS: TypedDataset[Event] = TypedDataset.create(adEventP.sample(N)).persist()

  eventDS.dataset.show

  ss.time(
    println(s"[Dataset] Prob(video) = ${eventDS.toDF.filter("type.duration is not null").count().toDouble / N}")
  )

  ss.time(
    println(s"[Prob] Prob(video) = ${adEventP.probability(_.`type`.isInstanceOf[Video], N)}")
  )

}
