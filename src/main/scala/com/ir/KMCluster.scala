package com.ir

import scala.collection.mutable
import scala.io.Source

/**
  *
  */
class KMCluster(num_of_clusters: String) {
  val embeddings = mutable.HashMap[String, Vector[Float]]()
  val k = num_of_clusters.toInt

  def read(file: String): mutable.HashMap[String, Vector[Float]] = {
    val lines = Source.fromFile(file)
      .getLines()
      .map(line => line.split(" "))

    for (line <- lines) {
      val word = line(0)
      val embedding = line.tail
        .map(x => x.toFloat)
        .toVector
      embeddings(word) = embedding
    }
    embeddings
  }

  /**
    * Returns mean vector of given vector list
    * @param vectors list of vectors
    * @return mean vector
    */
  def meanVector(vectors: List[Vector[Float]]): Vector[Float] = {
    val dimensionality = vectors.head.length
    var mVector = Vector.fill(dimensionality)(0.0.toFloat) //TODO enhance fill


    println(mVector.indices)
    println(mVector.size)
    println(mVector.length)

    for (vector <- vectors) {
      for (i <- 0 until dimensionality)
        mVector = mVector.updated(i, mVector(i) + vector(i))
    }

    mVector.map(sum => sum / vectors.length)

  }


  def euclidDistance(vector1: Vector[Float], vector2: Vector[Float]): Float = {
    Float.MinValue //TODO Implementation of calculation
  }


  def createCluster(vectors: List[Vector[Float]], centeroids: List[Vector[Float]]):
                                  mutable.HashMap[Vector[Float], List[Vector[Float]]] = {

    val cluster = mutable.HashMap[Vector[Float], List[Vector[Float]]]()

    for (vector <- vectors) {
      var min = Float.MaxValue


      for (centeroid <- centeroids) {
        val distance = euclidDistance(vector, centeroid)
        if (distance < min) {
          min = distance
          //TODO

        }
      }
    }
    cluster
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

      val result = kmc.meanVector(List( Vector(1.0.toFloat, 2.0.toFloat), Vector(3.0.toFloat, 5.0.toFloat))) //TODO delete
      println(result) //TODO delete


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
