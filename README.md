# FIB-LP-Kd2nTreeHaskell

This basic repository contains a vanilla implementation of kd 2n-Trees Structures in Haskell. The kd-Tree is a type of structure that provides us a simpler way to save a set of points (of a k-dimensions space). The present variation provides us a way to generalize the quadtrees and the octrees.

kd 2n-Trees are general search trees where each node contains one point of k dimensions, and one sorted list of coordinates of points, that is used for the distribution of the other points.

My implementation includes a generic definition of a type class Point, a definition of type kd 2n-Tree and several functions to work with the trees. There are some instances to guarantee different properties in all the code, including the instances of Applicative, Monad and Functor, in order to show different ways of using the structure.

