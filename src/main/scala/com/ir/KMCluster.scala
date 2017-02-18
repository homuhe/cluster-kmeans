package com.ir

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
  *
  * @param c
  */
class Cluster(c: Vector[Float]) {
  var words: List[String] = Nil
  var centroid = c
}

/**
  *
  */
class KMCluster(num_of_clusters: String) {
  val embeddings = mutable.HashMap[String, Vector[Float]]()
  var clusters = List[Cluster]()
  val k = num_of_clusters.toInt

  val thresholdLimit = 2

  /*
   */
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

  /*Random Centroid Pick
   */
  def pickRandomCentroids(): List[Vector[Float]] = {
    Random.shuffle(embeddings.values.toList).take(k)
  }

  /**
    * Returns mean vector of given vector list
    * @param vectors list of vectors
    * @return mean vector
    */
  def meanVector(vectors: List[Vector[Float]]): Vector[Float] = {
    val dimensionality = vectors.head.length
    var mVector = Vector.fill(dimensionality)(0.0.toFloat) //TODO enhance fill

    for (vector <- vectors) {
      for (i <- 0 until dimensionality)
        mVector = mVector.updated(i, mVector(i) + vector(i))
    }

    mVector.map(sum => sum / vectors.length)
  }

  /*
   */
  def euclidDistance(vector1: Vector[Float], vector2: Vector[Float]): Float = {
    var distance: Float = 0

    def square(x: Float) = x * x

    for (index <- vector1.indices) {
      distance += square(vector1(index) - vector2(index))
    }
    Math.sqrt(distance).toFloat
  }

  /*
   */
  def createClusters(centroids: List[Vector[Float]]) = {
    for (centroid <- centroids) {
      clusters = new Cluster(centroid) :: clusters
    }
  }

  def populateClusters(): Unit = {

    do {
      resetClusterWordSets()
      for (word <- embeddings) {
        var minCentroid = (0, Float.MaxValue)

        for (cluster <- clusters) {
          val dist = euclidDistance(word._2, cluster.centroid)

          if (dist < minCentroid._2) {
            minCentroid = (clusters.indexOf(cluster), dist)
          }
        }
        clusters(minCentroid._1).words = word._1 :: clusters(minCentroid._1).words
      }
    }while(updateClusterCentroids() != k)


  }

  def updateClusterCentroids() : Int = {
    var reachedThreshold = 0

    for(cluster <- clusters){
      val centroidDummy = cluster.centroid
      var wordVecList = List[Vector[Float]]()
      cluster.words.foreach(word => wordVecList = embeddings(word) :: wordVecList)
      cluster.centroid = meanVector(wordVecList)

      if(euclidDistance(centroidDummy, cluster.centroid) < thresholdLimit){
        reachedThreshold += 1
      }
    }
    println(reachedThreshold)
    reachedThreshold
  }

  def resetClusterWordSets() = {
    clusters.foreach(cluster => cluster.words = Nil)
  }

    for(cluster <- clusters){
      var wordVecList = List[Vector[Float]]()

      cluster.words.foreach(word => wordVecList = embeddings(word) :: wordVecList)
      cluster.centroid = meanVector(wordVecList)
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
      println("\nDONE!")

      kmc.createClusters(kmc.pickRandomCentroids())

      kmc.populateClusters()
      kmc.clusters.foreach(cluster => println("\n" + cluster.words))

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
