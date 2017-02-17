package com.ir

import scala.collection.mutable
import scala.io.Source

/**
  *
  */
class KMCluster(num_of_clusters: String) {
  val embeddings = mutable.HashMap[String, List[Float]]()
  val k = num_of_clusters.toInt

  var min = 0.0
  var max = 0.0

  def read(file: String): mutable.HashMap[String, List[Float]] = {
    val lines = Source.fromFile(file)
      .getLines()
      .map(line => line.split(" "))

    for (line <- lines) {
      val word = line(0)
      val embedding = line.tail
        .map(x => x.toFloat)
        .toList
      embeddings(word) = embedding

      //min / max value calculation
      for (num <- embedding) {
        if (num <= min) min = num
        if (num >= max) max = num
      }
    }
    embeddings
  }

}

/**
  *
  */
object KMCluster {

  def main(args : Array[String]) {

    println( "FML" )

    if (args.length == 2) {

      val KMC1 = new KMCluster(args(1))
      val input = KMC1.read(args(0))

      println(input.size + " Einträge gelesen!")

      //input.foreach(entry => println(entry))

      println(KMC1.min + "\n" + KMC1.max)

      println("\nDONE!")
    }
    else help()
  }

  def help(): Unit = {
    println("Usage: ./cluster-kmeans arg1 arg2")
    println("\t\targ1: INPUT - text file with embeddings")
    println("\t\targ2: ARG2 - desired cluster size")
    sys.exit()
  }
}
