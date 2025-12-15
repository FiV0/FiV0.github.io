---
layout: post
title: WCOJ - Datalog and GenericJoin
comments: true
redirect_from: "/2025/12/15/wcoj-datalog-and-genericjoin/"
permalink: wcoj-datalog-and-genericjoin
---

*See how easy it is to extend the GenericJoin interface `PrefixExtender` for `or`, `and`and`not`*

Recapping from our [last post](wcoj-generic-join), the interface implemented for a data pattern clause backed by one of the indexes  (some permutation of an EAV index) is
```kotlin
interface PrefixExtender : LevelParticipation {
    fun count(prefix: Prefix): Int
    fun propose(prefix: Prefix) : List<Extension>
    fun intersect(prefix: Prefix, extensions: List<Extension>) : List<Extension>
}
```
So far we have assumed any clause is just a standard data pattern clause  `[entity attribute variable]` which can be directly matched against one of the `EAV`, `AEV` or `VAE` indexes, but Datalog is more expressive than that.
### Or
With an `or` clause you can express that logic variables satisfy one or more clauses. So for example the following query finds people that have a last name of `Lovelace` and either have a first-name of `Ada` *or* are of the gender male. The restriction on `or` is that the set of variables appearing in the different child clauses must be identical. 
```clojure
{:find [p]
 :where [[p :last-name "Lovelace"]
         (or [p :first-name "Ada"] 
             [p :gender :male])]}
```
A `GenericOrPrefixExtender`[^1] can be recursively defined on nested `PrefixExtender`s. The logic mainly derives from the fact that an `or` is the union of all its sub-clauses.
```kotlin
open class GenericOrPrefixExtender(val children: List<PrefixExtender>) : PrefixExtender {

    override fun count(prefix: Prefix) = children.sumOf { it.count(prefix) }

    override fun propose(prefix: Prefix) = children.flatMap { it.propose(prefix) }.distinct()

    override fun intersect(prefix: Prefix, extensions: List<Extension>): List<Extension> {
        val result = mutableListOf<Extension>()
        for (child in children) {
            val childExtensions = child.intersect(prefix, extensions)
            result.addAll(childExtensions)
        }
        return result.distinct()
    }

    // All or clauses have the same variables, hence participate in the same levels
    override fun participatesInLevel(level: Int) = children.first().participatesInLevel(level)
}
```
In the extreme case all children propose different extensions so `count` is the sum of its children counts. For `propose` we take the proposals from all its children and assure we are not proposing candidates twice (`distinct`). Similarly for `intersect`, we intersect the given extensions with all the child clauses and assure uniqueness with `distinct`. The `distinct` logic could likely be better optimised as the implementation may currently create large intermediate lists. For example if extensions are sorted one could do a two-way merge for the set intersection. The complexity of the WCOJ is not affected by leaving this optimisation on the table.
### And
At the toplevel (and other logical connectors) conjunction is the default, so you don't need an explicit `and` clause, but you may need so inside an `or` clause.
```clojure
{:find [p]
 :where [[p :last-name "Lovelace"]
         (or [p :first-name "Ada"] 
             (and [p :first-name "Alan"] 
                  [p :gender :male]))]}
```
The above query finds people whose last name is "Lovelace" and who either have a first name of `Ada` *or* have first name of `Alan` *and* are also male.
The corresponding `GenericAndPrefixExtender`
```kotlin
open class GenericAndPrefixExtender(val children: List<PrefixExtender>) : PrefixExtender {

    // The count is the minimum of counts of all children
    override fun count(prefix: Prefix) = children.minOf { it.count(prefix) }

    override fun propose(prefix: Prefix) : List<Extension> {
        val nextLevel =  prefix.size
        val participants = children.filter { it.participatesInLevel(nextLevel) }
        val minChild = participants.minBy { it.count(prefix) }
        var extensions = minChild.propose(prefix)
        for (child in participants) {
            if (child != minChild) {
                extensions = child.intersect(prefix, extensions)
            }
        }
        return extensions
    }

    override fun intersect(prefix: Prefix, extensions: List<Extension>): List<Extension> {
        var currentExtensions = extensions
        val nextLevel = prefix.size
        // We could also only find the minChild here, but I thought the almost constant sort here wouldn't hurt much 
        val participants = children.filter { it.participatesInLevel(nextLevel) }.sortedBy { it.count(prefix) }
        for (child in participants) {
            currentExtensions = child.intersect(prefix, currentExtensions)
        }
        return currentExtensions
    }

    override fun participatesInLevel(level: Int) = children.any { it.participatesInLevel(level) }
}
```
In the `and` extender we are almost doing the same logic as in the `GenericSingleJoin` code from the previous post which makes sense as conjunction is implicit at the top-level of `:where`. An upper bound for the `count` is the smallest `count` of one of its children. In `propose` and `intersect` we assure that we start the intersection from the smallest (in size) child, similarly to the general`GenericJoin` to guarantee worst-case optimality.
### Not
The `not` clause is the [anti-join](https://en.wikipedia.org/wiki/Join_(relational_algebra)#Antijoin) of Datalog. You can use it to remove tuples that match the positive part of the query.
```clojure
{:find [p]
 :where [[a :last-name "Lovelace"]
         [a :first-name "Ada"]
         [a :gender g]
         [p :last-name "Lovelace"]
         (not [p :gender g])]}
```
The above query finds people of the Lovelace family that have a different gender to [Ada Lovelace](https://en.wikipedia.org/wiki/Ada_Lovelace) . In EDN Datalog all variables inside the `not` clause must be bound outside the `not` scope (formally this is known as [stratified Datalog](https://en.wikipedia.org/wiki/Stratification_(mathematics)#In_mathematical_logic) and will become important later on). For example one cannot write something like
```clojure
{:find [p]
 :where [(not [p :profession :programmer])]}
```
to find all non-programmers in the database.  For our current purposes this means that the following `GenericNotPrefixExtender` will (should) never `propose` extensions.
```kotlin
class GenericNotPrefixExtender(val children: List<PrefixExtender>, val level: Int): PrefixExtender {
    override fun count(prefix: Prefix): Int = Int.MAX_VALUE

    // If propose is called on NOT it means that the variable was not bound outside of NOT
    override fun propose(prefix: Prefix): List<Extension> {
       throw IllegalStateException("Propose should not be called on NOT prefix extender")
    }

    override fun intersect(prefix: Prefix, extensions: List<Extension>): List<Extension> {
        val prefixAndExtensionsExtender = PrefixExtender.createPrefixAndExtensionsExtender(prefix, extensions)
        val extensionsToRemove: Set<Extension> = GenericJoin(children + prefixAndExtensionsExtender, prefix.size + 1).join().map { resultTuple -> resultTuple.last() }.toHashSet()

        return extensions.filterNot { ext -> extensionsToRemove.contains(ext) }
    }

    override fun participatesInLevel(level: Int) = this.level == level
}
```
By setting the `count` conceptually to $\infty$ (just `MAX_VALUE` in reality) we are assuring that `propose` will never get called. `propose` will throw if called. The `level` the extender participates in is the last level of the variables participating in the `not` clauses. So for the `not` example query above, assuming the variable order is (`a`, `g`, `p`) , the not prefix extender would only participate in the 3rd level (after `g` is bound and going through extensions of `p`). In `intersect` we are making recursively use of `GenericJoin`.  The strategy is to treat the `not` clause as an inner join that tells us what to exclude. We construct a synthetic extender (`createPrefixAndExtensionsExtender`) representing our current prefix and candidate extensions, join it with the negated clauses, and remove any extensions that matched. The synthetic extender constrains the inner join to the prefix values already built, proposing exactly one value at each prefix level and then offering candidate extensions at the final level. See the [Appendix](#appendix) for the implementation of this synthetic extender.

Note that this is quite a naive implementation of an anti-join. In the inner `GenericJoin` we could for example restrict the prefixes to the variables actually participating in the `not` clauses as other variables won't get constrained by anything in recursive join (only take prefixes of `g` and `p` in our example above).

Complexity-wise all these 3 implementations are doing work that is bound by the size of the query (the number of sub-clauses in `or`, `and` and `not`respectively ) which can be considered constant for a fixed query. The work done at the bottom, actual triple clauses, is then by courtesy of the previous post, worst-case optimal. 

Up next we will do a little detour into [DBSP](https://arxiv.org/abs/2203.16684), an incremental computation framework, that efficiently incrementalizes all fundamental operators in relational algebra. This will then set us up for our final boss post where DBSP meets WCOJ.

### Appendix

This is the code for the synthetic prefix extender used to constrain the results in the inner join of the `GenericNotPrefixExtender`. 
```kotlin
interface PrefixExtender : LevelParticipation {
    ...
    
    companion object {

        fun createPrefixAndExtensionsExtender(fixedPrefix: Prefix, fixedExtensions: List<Extension>): PrefixExtender {
            val extensionSet = fixedExtensions.toHashSet()
            return object: PrefixExtender {
                private fun isPrefixMatching(prefix: Prefix): Boolean =
                    prefix.size <= fixedPrefix.size && fixedPrefix.take(prefix.size) == prefix

                override fun count(prefix: Prefix): Int =
                    if (isPrefixMatching(prefix))
                        if (prefix.size < fixedPrefix.size) 1 else fixedExtensions.size
                    else 0

                override fun propose(prefix: Prefix): List<Extension> =
                    if (isPrefixMatching(prefix))
                        if (prefix.size < fixedPrefix.size) listOf(fixedPrefix[prefix.size]) else fixedExtensions
                    else
                        emptyList()

                override fun intersect(prefix: Prefix, extensions: List<Extension>): List<Extension> =
                    if (isPrefixMatching(prefix))
                        if (prefix.size < fixedPrefix.size)
                            if (extensions.contains(fixedPrefix[prefix.size]))
                                listOf(fixedPrefix[prefix.size])
                            else
                                emptyList()
                        else
                            extensions.filter { ext -> extensionSet.contains(ext) }
                    else
                        emptyList()

                override fun participatesInLevel(level: Int) = level <= fixedPrefix.size
            }
        }
    }
}
```

[^1]: You can find the code of this post at <https://github.com/FiV0/hooray2>
