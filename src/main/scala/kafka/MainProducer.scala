package kafka

import com.ovoenergy.kafka.serialization.avro4s._
import com.ovoenergy.kafka.serialization.core._
import org.apache.kafka.clients.CommonClientConfigs._
import org.apache.kafka.clients.producer.{ KafkaProducer, ProducerRecord }
import com.sksamuel.avro4s._
import main.AvroEvent
import AvroEvent.fromEvent
import main.MainUser.eventProb

import scala.collection.JavaConverters._

object MainProducer extends App {
  val schemaRegistryEndpoint = "http://localhost:18081"

  val topic = "avro-event"

  implicit val avroEventToRec: ToRecord[AvroEvent] = ToRecord[AvroEvent]

  val producer: KafkaProducer[String, AvroEvent] = new KafkaProducer(
    Map[String, AnyRef](BOOTSTRAP_SERVERS_CONFIG -> "localhost:9092,localhost:9093").asJava,
    nullSerializer[String],
    avroBinarySchemaIdSerializer[AvroEvent](schemaRegistryEndpoint, isKey = false, includesFormatByte = true)
  )

  while (true) {

    val events: Seq[AvroEvent] = eventProb.sample(100).flatten.map(fromEvent)

    events foreach { ev ⇒
      producer send new ProducerRecord[String, AvroEvent](topic, ev)
    }
  }
}
