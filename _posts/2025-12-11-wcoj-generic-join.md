---
layout: post
title: WCOJ - Generic Join
comments: true
redirect_from: "/2025/12/11/wcoj-generic-join/"
permalink: wcoj-generic-join
---

*Explaination of a worst-case-optimal variable oriented join algorithm - GenericJoin [^2]*

We discussed what Worst-case optimal join (WCOJ) is in a [previous post](/wcoj-graph-join-correspondence). Today we are going to try to convey the intuitions behind an actual WCOJ algorithm. Most of the code will be in Kotlin [^1]. A WCOJ is a join that is variable oriented instead of relation oriented. What do I mean by that? Coming back to our beloved triangle query (You really need love this query as it will stay with us for quite a while).

$$Q(A,B,C) = R(A,B) \bowtie S(B,C) \bowtie T(C,A)$$

You can join by relation by doing standard binary joins $(R \bowtie S) \bowtie T$ and producing tuples `(a, b, c)` after $R \bowtie S$ and then only validate against $T$. As we have seen previously the intermediate result might be quite large compared to the final output size. In a variable oriented join you add variables one at a time. So in case of the triangle query you first start by adding the $a_i$ that match both $R$ and $T$ and then extend the prefixes of $(a_i)$ with matching $b_j$s to prefixes $(a_i \space b_j)$. This approach assumes that you have an index of `AB` for $R$, to efficiently look up $b$ given an $a$.

This is also why I quite like the triangle query in Datalog as an example when explaining WCOJ.
```clojure
{:find [?a ?b ?c]
 :where [[?a :g/to ?b]
         [?a :g/to ?c]
         [?b :g/to ?c]]}
```

The variables are explicit and in [Datomic](https://www.datomic.com/ ) and [XTDB v1](https://v1-docs.xtdb.com/main/) the indices `EAV`, `AEV` and `AVE` are also be default part of the database. Prefix lookups (given a bound variable`?a` you can efficiently find `?b` via the `AEV` index) are therefore possible (for most scenarios).

The following interface is shamelessly stolen from a very good [blog post]() on Generic Join.
```kotlin
typealias Prefix = List<Any>
typealias Extension = Any

interface PrefixExtender : LevelParticipation {
    fun count(prefix: Prefix): Int
    fun propose(prefix: Prefix) : List<Extension>
    fun intersect(prefix: Prefix, extensions: List<Extension>) : List<Extension>
}
```
For any triple clause in the query (`[?a :g/to ?b]` for the triangle query) we have this interface backed by one of the `EAV`, `AEV` or `VAE` indexes (`AEV` for `[?a :g/to ?b]` as the attribute part is fixed and $A$ comes before $B$ in the variable order). A `Prefix` is just what we called a prefix above, a prefix of a potential result row. An `Extension` is what could potentially be added to a prefix to become a new longer prefix. So for the triangle case we would have for example a prefix $(a_i \space b_j)$ once we joined variables $A$ and $B$ and an extension $c_k$ when doing the join on $C$.
- `count` returns how many extensions there are for a given prefix.  As an example if the Datalog query contains the triple clause `[?person :person/name ?name]` and we are given the prefix `(1)` (just the entity id 1),  then the `count` would return the prefix count for `[:person/name 1]` in the `AEV` index. The attribute `:person/name` is fixed across the whole query so we won't be part of the prefix, but just get fixed on the initialization of the `PrefixExtender`
- `propose` actually returns the extensions for a give prefix. For the `AEV` example above this will likely be just a name; a list of size one. If the attribute has multiplicative cardinality (as it the case for `g/to`) then the result will be all out-going edges from the already fixed node.
- `intersect` intersects a given list of extensions with the extensions of this particular `PrefixExtender`.

Let's now describe the algorithm for a single variable level. For the join variable order $(A, B, C)$ and the triangle query, $R$ participates in levels 1 and 2 and $T$ participates in levels 1 and 3. In order for the algorithm to remain worst-case optimal (WCO) we need to start with the smallest extension set. Take the smallest extension set as a initial guess and then refine this initial guess based on the other `PrefixExtender`'s participating on that level.

In code:
```kotlin
class GenericSingleJoin(val extenders : List<PrefixExtender>, val prefixes: List<Prefix>) : Join<Prefix> {

    override fun join(): List<Prefix> {
        val results = mutableListOf<Prefix>()
        for (prefix in prefixes) {
            // For every prefix find the extender that proposes the least extensions
            val minIndex = extenders.indices.minBy { extenders[it].count(prefix) }
            // Propose extensions from that extender
            var extensions = extenders[minIndex].propose(prefix)
            // Intersect with all other extenders
            for (i in extenders.indices) {
                if (i != minIndex) {
                    extensions = extenders[i].intersect(prefix, extensions)
                }
            }
            results.addAll(applyExtensions(prefix, extensions))
        }
        return results
    }
}
```

`applyExtensions` is function that for a given prefix creates a new set of result prefixes, the prefix concatenated with each extension. The important part, the one that guarantees us worst-case optimality,  is that the intersection starts of with the minimal set of extensions. Subsequent intersections can then all be done in time proportional to this smallest set. The actual proof of this claim is part of the paper introducing GenericJoin[^2].

To see why the smallest intersection choice matters, consider extending a prefix $(a_i, b_j)$ to find valid $c$ values in the triangle query. Relation $S$ proposes all nodes reachable from $b_j$, while $T$ proposes all nodes that have edges back to $a_i$. If $b_j$ is a high-degree node with many outgoing edges but only a few of those destinations connect back to $a_i$, starting with $S$'s large proposal wastes work. By always beginning with the smallest proposer, the total intersection work is bounded by $(\text{min count}) \times (\text{number of relations})$ rather than being dominated by the largest relation's contribution.

One other important part for this algorithm to work efficiently is that the relations involved are indexed by the variables appearing in the prefix (to efficiently look up a proposal and do intersection with previous extensions). This is most likely not the case for your average SQL table, but it is the case for most variations of Entity-Attribute-Value triple indices.

Now putting the whole algorithm together, it mainly remains to call `GenericSingleJoin` at every level.
```kotlin
class GenericJoin(val extenders: List<PrefixExtender>, levels: Int) : Join<ResultTuple> {

    // Precompute which extenders participate in which levels
    private val extenderSets : List<List<PrefixExtender>> = List(levels) { level ->
        val participants = mutableListOf<PrefixExtender>()
        for (extender in extenders) {
            if (extender.participatesInLevel(level)) {
                participants.add(extender)
            }
        }
        participants
    }

    override fun join(): List<ResultTuple> {
        var prefixes: List<Prefix> = listOf(emptyList())

        // For every level, perform a single join with the extenders participating in that level
        for (extenderSet in extenderSets) {
            val singleJoin = GenericSingleJoin(extenderSet, prefixes)
            prefixes = singleJoin.join()
        }
        return prefixes
    }
}
```

The extended prefixes at one level become the new prefixes at the next level and we start with an empty prefix.
At the last level the prefixes are just the result tuples.
And that's it, in roughly ~50 LOC (modulo all the stuff I have left out) we have gotten in implementation of a WCOJ algorithm.
In a next post will see how the `PrefixExtender` interface composes nicely for certain logical connectors of Datalog like `not`, `and` and `or`.

[^1]: You can find the code from this post at <https://github.com/FiV0/hooray2>

[^2]: The algorithm explained in this post is called GenericJoin and from the following paper <https://arxiv.org/abs/1310.3314.>
