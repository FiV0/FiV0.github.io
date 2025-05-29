---
layout: post
title: XTDB's transactional model
comments: true
redirect_from: "/2025/05/29/xtdbs-transactional-model/"
permalink: xtdbs-transactional-model
---


[Alex Miller](https://transactional.blog/about) recently wrote a good blog post in which he details how to [decompose transactional systems](https://transactional.blog/blog/2025-decomposing-transactional-systems) according to the following steps:

> Every transactional system does four things:
>
> * It _executes_ transactions.
> * It _orders_ transactions.
> * It _validates_ transactions.
> * It _persists_ transactions.

In what follows, we will attempt to do just do that for XTDB (henceforth XT).
Even though [XT v1](https://v1-docs.xtdb.com/) and [XT v2](https://docs.xtdb.com/) are quite different on user level, the architecture according to the above is roughly the same across the two versions.

> *Ordering* a transaction means assigning the transaction some notion of a time at which it occurred.

> *Persisting* a transaction makes it durable, generally to disk.

XT is actually very similar to Calvin system described in the original post.
XT submits the transactions to a global log which *orders* and *persists* them (think of a Kafka log).
The submit timestamp of the log is the [system time](https://docs.xtdb.com/quickstart/sql-overview.html#system-time-columns-automatic-time-versioning-of-rows-without-audit-tables) of the records going into XT.
Persistence here means the transaction is persisted and not the results of the transaction.
We therefore acknowledge a transaction when it hits the log (commit), but it still needs to get indexed.
The disadvantages are the same as for Calvin, *in that long running transactions will stall any later committed transactions from executing*.
XT and Calvin don't support interactive (session-style) transactions either.

> *Validating* a transaction means enforcing concurrency control, or more rarely, domain-specific semantics.

Each XT node reads transactions from the log and *validates* that the transaction timestamp is strictly greater then the last submitted transaction.
We also allow specifying a system time when submitting a transaction for backfilling systems and this case reject transactions with out of order manually specified timestamps.

As every transaction is processed in order on every node there is no Multiversion Concurrency Control.
At some point in the future we will could add sharding (potentially via multiple Kafka topics).

> *Executing* a transaction means evaluating the body of the transaction to produce the intended reads and writes.

After validation, each node *executes* and indexes the transaction into the live index (an in memory structure) and eventually into a block which gets persisted to disk.
If a node shuts down or fails before the results of a transaction are written to disk, the live index can be rebuild from the log.

![XTDB's transaction model](/assets/xtdb_transactional_model.excalidraw.png)

You might be wondering where the coordination occurs in XT's scenario.
As every node is indexing every transaction in order, the burden of coordination lies with the log and is outsourced in this case (Kafka as of this writing).
