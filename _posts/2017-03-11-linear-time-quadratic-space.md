---
layout: post
title: Linear Time, Quadratic Space
comments: true
redirect_from: "/2017/03/11/linear_time_quadratic_space/"
permalink: linear-time-quadratic-space
---

Whenever an algorithm needs some amount of space $S$, the algorithm runs at least in $S$ time because why allocate the space
if it is not used ? This post tries to show that this is a misconception. My supervisor [Michel Habib](https://www.irif.fr/~habib/) showed me the following neat little trick recently.

Consider as an example the function `malloc()`
which allocates a certain amount of space without initializing it (contrary to `calloc()`
).
Depending on the current state of the memory manager and the implementation, a call to `malloc()`
could therefore be contant. Simplifying a lot of things, one could tell the memory manager the beginning and the end of the momory block used.

Whenever a graph algorithm wants to check in constant time if an edge exists, it usually needs to create the whole adjacency matrix. In the following we show that this is not strictly necessary. Let $G$ be a griven graph.
We allocate a matrix $M$ of size $n \times n$ without initializing it. Let $E$ be the list of edges of the graph $G$.
For every edge $(i,j)$ we create a pointer to the entry
$$M_{ij}$$ and $$M_{ij}$$ points to the edge $(i,j)$. This initialization can be done in linear time $(O(n+m))$.
The process only makes sense if the number of edges is subquadratic.

![Illustration](assets/edge-matrix.svg ){: .center-image }

If the algorithm wants to check if an edge exists it looks at the entry $M_{ij}$.
If $$M_{ij}$$ points to some garbage the algorithm knows that the edge is non-existant.
Otherwise the entry points to some edge $(i,j)$ (it might have pointed there by some unlucky coincidence).
The algorithm verifies that the edge exists by checking
that the pointer of $(i,j)$ is pointing back to $$M_{ij}$$.
This yields a data structure to check for the existant of an edge in constant time with only linear precomputation.

