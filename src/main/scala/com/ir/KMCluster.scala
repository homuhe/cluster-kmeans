package com.ir

import scala.collection.mutable
import scala.io.Source

/**
  *
  */
object KMCluster {

  def main(args : Array[String]) {

    println( "FML" )

    val input = read(args(0))

    input.foreach(entry => println(entry))

  }

  def read(file: String): mutable.HashMap[String, List[Float]] = {
    val embeddings = mutable.HashMap[String, List[Float]]()

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
