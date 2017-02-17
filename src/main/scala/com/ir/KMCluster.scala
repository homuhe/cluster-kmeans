package com.ir

import scala.collection.mutable
import scala.io.Source

/**
  *
  */
class KMCluster(num_of_clusters: String) {
  val embeddings = mutable.HashMap[String, List[Float]]()
  val k = num_of_clusters.toInt
  val tol = 0.001
  val max = 50

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
