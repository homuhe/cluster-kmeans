package com.ir

import scala.collection.mutable
import scala.io.Source

class Cluster(c: Vector[Float]) {
  val words = mutable.Set
  val centeroid = c
}

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

  def initDimensionality = embeddings.head._2.length

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

    //test me plz
    var distance: Float = 0
    for (index <- vector1.indices) {
      distance += square(vector1(index) - vector2(index))
    }



    def square(x: Float) = x * x

    Math.sqrt(distance).toFloat
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

      val vec1 = Vector(1.0.toFloat, 2.0.toFloat, 0.toFloat, 0.toFloat, 0.toFloat, 0.toFloat)
      val vec2 = Vector(3.0.toFloat, 5.0.toFloat, 10.toFloat, 20.toFloat, 30.toFloat, 40.toFloat)

      println(kmc.meanVector(List( vec1, vec2))) //TODO delete

      println(kmc.euclidDistance(vec1, vec2))

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
