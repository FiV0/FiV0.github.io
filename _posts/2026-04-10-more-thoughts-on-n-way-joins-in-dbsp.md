---
layout: post
title: More thoughts on N-way joins in DBSP
comments: true
redirect_from: "/2026/04/10/more-thoughts-on-n-way-joins-in-dbsp/"
permalink: more-thoughts-on-n-way-joins-in-dbsp
---

*Thoughts on N-way joins in DBSP and WCOJ in DBSP after some discussion on the [Feldera](https://github.com/feldera/feldera) Discord server*

I used the following recursive formula for multi-joins in [Efficient multi-joins in DBSP](wcoj-dbsp-zsets-and-datalog#efficient-multi-joins-in-dbsp)

$$
\begin{align*}
\Delta_{1..i} &= \Delta_{1..i-1} \bowtie \Delta R_i^A \\
              &+ \Delta_{1..i-1} \bowtie (R_i^A)^{-1} \\
              &+ \Delta R_i^A \bowtie (R_1^A)^{-1} \bowtie \dots \bowtie (R_{i-1}^A)^{-1}
\end{align*}
$$

From a discussion on the Feldera Discord[^1] I learned that Feldera uses the following formula for multi-way joins. For completeness $A^\Delta$ is the delta for relation $A$ arriving at the current batch and $A^{-1}$ is $A$ just before the current batch has been processed. Let's say you want to compute the incremental query $(A \bowtie B \bowtie C)^\Delta$. This can be written as 

$$
A^\Delta \bowtie B \bowtie C \enspace + \enspace A^{-1} \bowtie B^\Delta \bowtie C \enspace + \enspace A^{-1} \bowtie B^{-1} \bowtie C^\Delta
$$

you obtain the above by "collapsing" certain terms together from the fully expanded version using the bilinear formula.  The first term is obtained by collapsing

$$
\begin{align*}
A^\Delta \bowtie B^\Delta \bowtie C^\Delta \enspace &+ \enspace A^\Delta \bowtie B^{-1} \bowtie C^\Delta \enspace + \enspace \\ 
A^\Delta \bowtie B^\Delta \bowtie C^{-1} \enspace &+ \enspace
A^\Delta \bowtie B^{-1} \bowtie C^{-1}
\end{align*}
$$

Any terms involving $A^\Delta$ are merged into the first term. For the second you merge everything that contains a $B^\Delta$ but no $A^\Delta$ and so forth. The formula for an N-way join then becomes 

$$
\sum_{i \in [n]} R_1^{-1}\dots R_{i-1}^{-1} R_i^\Delta R_{i+1} \dots R_n
$$

I am abusing the Cartesian product notation here for join. I mainly wondered if we could make use of this kind of "collapsing" as well in the recursive formula above (by collapsing the first two terms)

$$
\begin{align*}
\Delta_{1..i} &= \Delta_{1..i-1} \bowtie R_i^A \\
              &+ \Delta R_i^A \bowtie (R_1^A)^{-1} \bowtie \dots \bowtie (R_{i-1}^A)^{-1}
\end{align*}
$$

The problem is that we still need $(R_{i-1}^A)^{-1}$ terms in later iterations, so we can't simply merge $R_i^{\Delta}$ into $R_i^{-1}$ as we still need the $R_i^{-1}$ later on. 

If you flip the order around (this is just a renaming of relations), you obtain:

$$
\sum_{i \in [n]} R_i^\Delta R_1 \dots R_{i-1}  R_{i+1}^{-1} \dots R_n^{-1}
$$

I wonder if this is actually simpler to implement than the recursive formula above. After having processed $R_i^{\Delta}$, you merge $R_i^{\Delta}$ into $R_i^{-1}$ to obtain $R_i$. This works because in subsequent iterations of that loop $R_i^{-1}$ is no longer needed. The main difference for us vs. the Feldera folks is that we are not hardcoding (compiling) the circuit at this level based on the number of relations involved. We want this circuit component to work for an arbitrary number of relations (essentially the triple patterns of our Datalog queries). Feldera would have for example 3 different circuit parts for the $(A \bowtie B \bowtie C)^\Delta$ query above , whereas our algorithm is the same no matter if there are 3, 4 or 5 relations involved. 

In the limit the recursive formula does fewer joins (only about half as many). The problem is that this only amortizes after 5 relations are involved, before that the simpler formula does *fewer* joins. So this is something to verify and hopefully also run some benchmarks against. I suspect that the simpler formula might actually be faster in practice.

For Feldera there is actually an optimisation problem to be solved here. In our EDN Datalog case we have all possible permutations of our [covering indices](https://docs.datomic.com/indexes/indexes.html) at hand and so prefix lookups on the permuations of the covering index work not matter the variable order. Feldera doesn't have this luxury. So they might need to build extra indices for joining certain terms. If you now add parallelisation to the mix (joining data on different workers), you need to also decide which (potentially newly build) index permutations do you send to which worker. This is I think a good topic for another blog post. That's all for now.

[^1]:<https://discord.com/channels/1223851723110748251/1223851723643293729/1458497257304363186>

