package com.ir

import scala.collection.mutable
import scala.io.Source


class KMCluster {
  val embeddings = mutable.HashMap[String, List[Float]]()

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

    val KMC1 = new KMCluster
    val input = KMC1.read(args(0))

    input.foreach(entry => println(entry))

  }

}
