
/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  * Task:         Assignment 5A
  * Description:  K-Means Clustering Algorithm
  */

package com.ir

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

/**
  * Cluster class
  * @param c centroid vector
  */
class Cluster(c: Vector[Float]) {
  var words: List[String] = Nil
  var centroid = c
}

/**
  * k-means cluster class
  * @param num_of_clusters number k of desired clusters
  */
class KMCluster(num_of_clusters: String) {
  val embeddings = mutable.HashMap[String, Vector[Float]]()
  var clusters = List[Cluster]()
  val k = num_of_clusters.toInt

  // threshold 1 will guarantee only 1 iteration
  // (quick run, results depend on random starting centroids)
  // threshold below 0.01 can take more than 10+ iterations
  // (long, but more accurate run)
  var thresholdLimit = 0.5


  /**
    * Reads the file in and produces a representation in memory
    * @param file the file containing the word embeddings
    * @return a map from word to its vector
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
      val l2Norm = L2Norm(embedding)
      embeddings(word) = embedding.map(vec => vec/l2Norm)
    }
    embeddings
  }

  // returns k random starting data points for cluster centroids
  def pickRandomCentroids(): List[Vector[Float]] = Random.shuffle(embeddings.values.toList).take(k)


  /**
    * Returns mean vector of given vector list
    * @param vectors list of vectors
    * @return mean vector
    */
  def moveCentroid(vectors: List[Vector[Float]]): Vector[Float] = {
    val dimensions = vectors.head.length
    var mVector = Vector.fill(dimensions)(0.0.toFloat) //TODO enhance fill

    for (vector <- vectors) {
      for (i <- 0 until dimensions)
        mVector = mVector.updated(i, mVector(i) + vector(i))
    }

    mVector.map(sum => sum / vectors.length)
  }


  /**
    * Returns the euclidean distance between two vectors
    * @param vector1 list of vector 1
    * @param vector2 list of vector 2
    * @return euclidean distance
    */
  def euclidDistance(vector1: Vector[Float], vector2: Vector[Float]): Float = {
    var distance: Float = 0

    for (index <- vector1.indices) {
      distance += square(vector1(index) - vector2(index))
    }
    Math.sqrt(distance).toFloat
  }

  // calculating the L2-norm for unit length normalization
  def L2Norm(vector: Vector[Float]): Float = Math.sqrt(vector.map(x => square(x)).sum).toFloat


  // helper method to square
  def square(x: Float) = x * x

  /**
    * Initializing the clusters
    * @param centroids list of (random) centroids vectors
    */
  def createClusters(centroids: List[Vector[Float]]) = {
    for (centroid <- centroids) {
      clusters = new Cluster(centroid) :: clusters
    }
  }

  /**
    * clustering algorithm
    * firstly initializes the clusters and then several iterations can occur
    * depending on the threshold that has been set, but at least one
    */
  def populateClusters(): Unit = {
    createClusters(pickRandomCentroids())

    do {
      resetClusterWordLists()
      for ((word, vector) <- embeddings) {
        var minCentroid = (0, Float.MaxValue)

        for (cluster <- clusters) {
          val dist = euclidDistance(vector, cluster.centroid)

          if (dist < minCentroid._2) {
            minCentroid = (clusters.indexOf(cluster), dist)
          }
        }
        //assign word with smallest to a cluster to that cluster
        clusters(minCentroid._1).words = word :: clusters(minCentroid._1).words
      }
    } while(updateClusterCentroids() != k)
  }

  /**
    * changes the position of the centroids, according to
    * the new data points of the words that have been assigned to
    * the cluster
    * @return number of centroid that did not move very much compared to previous position
    */
  def updateClusterCentroids() : Int = {
    var reachedThreshold = 0

    for(cluster <- clusters){
      val centroidDummy = cluster.centroid
      var wordVecList = List[Vector[Float]]()
      cluster.words.foreach(word => wordVecList = embeddings(word) :: wordVecList)
      cluster.centroid = moveCentroid(wordVecList)

      if(euclidDistance(centroidDummy, cluster.centroid) < thresholdLimit){
        reachedThreshold += 1
      }
    }
//    println(reachedThreshold + " centroids reached required minimal distance improvement (threshold)")
    reachedThreshold
  }

  // emptying the cluster word lists again
  def resetClusterWordLists() = clusters.foreach(cluster => cluster.words = Nil)
}

/**
  * Companion object of KMCluster class
  */
object KMCluster {

  def main(args : Array[String]) {

    if (args.length == 2 || args.length == 3) {

      // declare k-means clustering with parameter k
      val kmc = new KMCluster(args(1))

      // read in the file, instantiate word embeddings map
      kmc.read(args(0))

      //optionally let the user assign a threshold himself
      if (args.length == 3) kmc.thresholdLimit = args(2).toFloat

      // start actual algorithm
      kmc.populateClusters()

      // print each word with its cluster
      for (cluster <-  kmc.clusters)
        for (word <- cluster.words) println(kmc.clusters.indexOf(cluster) + " " + word)

    }
    else help()
  }

  /**
    * Helper method
    */
  def help(): Unit = {
    println("Usage: ./cluster-kmeans arg1 arg2 [opt1]")
    println("\t\targ1: INPUT FILE\t - text file with embeddings")
    println("\t\targ2: INTEGER\t    - number of desired clusters")
    println("\t\t\t\t                 min = 1; max = number of words in input file")
    println("\t\topt1: FLOAT\t      - cluster movement tolerance, threshold to stop algorithm")
    println("\t\t\t\t                 recommended value: 1 - 0.001 (the higher the threshold the less iterations)")
    sys.exit()
  }
}
