package config

import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession

object SparkConfig {

  implicit val ss: SparkSession = SparkSession
    .builder()
    .config(new SparkConf().setAppName("yoProb").setMaster("local[*]"))
    .getOrCreate()
}
