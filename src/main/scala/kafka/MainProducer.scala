package kafka

import java.sql.Timestamp

import com.ovoenergy.kafka.serialization.avro4s._
import com.ovoenergy.kafka.serialization.core._
import org.apache.kafka.clients.CommonClientConfigs._
import org.apache.kafka.clients.producer.{ KafkaProducer, ProducerRecord }
import com.sksamuel.avro4s._
import main.AClic
import AClic.fromClic
import main.MainUser.clicsProb

import scala.collection.JavaConverters._

object MainProducer extends App {
  val schemaRegistryEndpoint = "http://localhost:18081"

  val topic = "avro-clic"

  implicit val clicToRecord: ToRecord[AClic] = ToRecord[AClic]

  val producer: KafkaProducer[String, AClic] = new KafkaProducer(
    Map[String, AnyRef](BOOTSTRAP_SERVERS_CONFIG -> "localhost:9092,localhost:9093").asJava,
    nullSerializer[String],
    avroBinarySchemaIdSerializer[AClic](schemaRegistryEndpoint, isKey = false, includesFormatByte = true)
  )

  while (true) {

    val clics: Seq[AClic] = clicsProb.sample(100).flatten.map(fromClic)

    clics foreach { clic ⇒
      producer send new ProducerRecord[String, AClic](topic, clic)
    }
  }
}
