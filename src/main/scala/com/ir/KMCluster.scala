package com.ir

import scala.collection.mutable
import scala.io.Source

/**
  *
  */
class KMCluster(num_of_clusters: String) {
  val embeddings = mutable.HashMap[String, List[Float]]()
  val k = num_of_clusters.toInt

  //var min = 0.0
  //var max = 0.0

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
      //for (num <- embedding) {
        //if (num <= min) min = num
        //if (num >= max) max = num
      //}
    }
    embeddings
  }

  /**
    * Returns mean vector of given vector list
    * @param vectors list of vectors
    * @return mean vector
    */
  def meanVector(vectors: List[List[Float]]): List[Float] = {
    val len = vectors.head.length
    var mVector = List.fill(len)(0.0.toFloat) //TODO enhance fill

    for (vector <- vectors) {
      for (i <- 0 until len)
        mVector = mVector.updated(i, mVector(i) + vector(i))
    }

    mVector.map(sum => sum / vectors.length)
  }

}

/**
  *
  */
object KMCluster {

  def main(args : Array[String]) {

    println( "FML" )

    if (args.length == 2) {

      val kmc = new KMCluster(args(1))
      val input = kmc.read(args(0))

      println(input.size + " Einträge gelesen!")

      val result = kmc.meanVector(List( List(1.0.toFloat, 2.0.toFloat), List(3.0.toFloat, 5.0.toFloat)))

      println(result)


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
