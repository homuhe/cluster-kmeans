# cluster-kmeans
k-means clustering

Creates clusters of given word embeddings. Underlying theory is [k-means clustering](https://en.wikipedia.org/wiki/K-means_clustering).

Assuming that word embeddings as vectors in a n-dimensional space have distances to each other, vectors of words with similar word embeddings will have smaller distances to each other. Placing cluster seeds aka centroids and gathering near vectors to such centroids will result in clusters of words with similar word embeddings.


## Input
Sample input file is a word embedding trained on German Wikipedia by using the word2vec CBOW model with vector size 300 and a minimum word frequency of 50.

Required format for using cluster-kmeans:
```
word1 value1 value2 value3 ...
word2 value1 value2 value3 ...
...
```

## Usage
```
$ ./cluster-kmeans arg1 arg2 [opt1]
    arg1: INPUT FILE  - text file with embeddings")
    arg2: INTEGER     - number of desired clusters
                        min = 1; max = number of words in input file")
    opt1: FLOAT       - cluster movement tolerance, threshold to stop algorithm
                        min = 0.1E-15, max = Float.MaxValue")
```
Example run:
```
$ ./cluster-kmeans input-sample.txt 100 0.1
> 0 Seit
0 Ab
0 Von
0 Zum
1 Dichter
1 Mensch
1 Historiker
...
```

## Output
Program prints each word and its cluster on a line, separating the word and the cluster number by a space.


_

Authors:

Alexander Hartmann, Holger Muth-Hellebrandt
