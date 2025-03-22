---
layout: post
title: Window functions in practice
comments: true
redirect_from: "/2025/03/22/window-functions-in-practice/"
permalink: window-functions-in-practice
---

*TLDR: Some practical information of how to implement window functions based on the paper: [Efficient Processing of Window Functions in Analytical SQL Queries, Leis et al.](https://vldb.org/pvldb/vol8/p1058-leis.pdf)*

Window functions allow you to compute values over a set of rows while preserving the original row. Unlike the `GROUP BY` operation, which aggregates the grouped rows into a single result, window functions maintain each row. This makes window functions ideal for calculating running totals, ranks, and moving averages.

Let us illustrate window functions with an example over sales data:
```sql
SELECT
  sale_id,
  region,
  SUM(sale_amount) OVER
      (PARTITION BY region
       ORDER BY _valid_from
       RANGE BETWEEN INTERVAL '1 DAY' AND CURRENT ROW)
       AS one_day_rolling_sum
FROM sales;
```

The query above computes the 1-day running total of sales per region.

In SQL a window function consists of three main parts:
- A `PARTIION BY` clause  specifies how the rows should be grouped, similar to `GROUP BY`, but without collapsing the groups. In the example above, the rows are partitioned by `region`.
- An `ORDER BY` clause defines the order in which rows are processed. In the example, `_valid_from` is used to indicate the time of the sale.
- A frame clause further refines the set rows to be considered around the current row in the calculation:
	- `ROWS BETWEEN` operates on the physical rows before and after the current row.
	- `RANGE BETWEEN` operates on logical rows based on the ordering column, such as time intervals or numeric ranges.

Window functions are executed after `GROUP BY`, `HAVING`, and other expressions in the `SELECT` clause, but before the `ORDER BY` clause of the entire query. Each component of a window function is optional. If the `PARTITION BY` clause is omitted, the entire result set is treated as a single partition. Without an `ORDER BY` clause, rows are processed in the order they are received. The default frame specification is `BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW`, which includes all rows from the start of the partition up to the current row.

Certain window function aggregates are unaffected by framing. Examples include `ROW_NUMBER()` (which provides the ordinal position within the partition), `RANK()` (which assigns ranks within the partition, handling ties by assigning the same rank and leaving gaps), and `DENSE_RANK()` (similar to `RANK()`, but without gaps for ties). On the other hand, aggregate functions like `SUM()` and analytical functions like `FIRST_VALUE()` do utilize the window frame to compute their results.

It's possible to calculate the values produced by window functions without actually using window functions. This often involves self joins and correlated subqueries. For instance, the example query above can be rewritten as follows:
```sql
SELECT
    s1.sale_id,
    s1.region,
    (
        SELECT SUM(s2.sale_amount)
        FROM sales s2
        WHERE s2.region = s1.region
          AND s2._valid_from BETWEEN s1._valid_from - INTERVAL '1 DAY' AND s1._valid_from
    ) AS daily_cumulative_total
FROM sales s1;
```
However, this approach can result in an $O(N^2)$ runtime if the window frame is large, compared to $O(N \log N)$ or $O(N)$ when the implementation is done smartly when shifting the frame (more on that below).
### Partitioning and sorting

There are two primary approaches to partitioning and sorting data for window functions:

- A hash-based approach first partitions the input data by the `PARTITION BY` attributes and then independently sorts each partition by the `ORDER BY` attributes.
- A purely sort-based approach combines the `PARTITION BY` and `ORDER BY` attributes and sorts the entire input.

Theoretically, the hash-based approach is faster because sorting only needs to occur within each partition after the $O(N)$ hash partitioning phase, compared to an $O(N \log N)$ overall sort. As sorting likely needs to happen anyway (it's possible to omit the `ORDER BY`), it can be simpler and more straightforward to implement.

Both approaches lend themselves well to parallelization. For the sort-based approach, a parallel sorting algorithm is sufficient. For parallel hash-partitioning each thread processes an independent chunk of the input data, creating hash groups for its chunk. Afterwards, the same hash groups from different chunks are combined into partitions. The sorting of these partitions can then be done independently for each chunk. If the partitions are large or heavily skewed, intra-partition parallelization via a parallel sort can be applied.

![window_functions_partitioning+sorting](assets/window_functions_partitioning+sorting.excalidraw.png){: .center-image }

The above illustrates the different phases of the hash-based approach with two partitions and two threads.
### Framing
Once partitioning and sorting are complete, you can explore strategies of evaluating the window aggregate per row. The naive implementation would evaluate the frame for every row, resulting in an $O(N^2)$ runtime (where $N$ is the size of the partition) if the frame is large relative to the partition.

Consider a running sum over `ROWS BETWEEN 5 PRECEDING AND 5 FOLLOWING` . Many database systems solve this with a removable cumulative approach. When the frame shifts the new element gets added to the aggregate and the element that has dropped of the frame gets removed.

![cumulative_simple](assets/cumulative_simple.excalidraw.png){: .center-image }

This works well for `SUM` and `AVG`, resulting in a linear runtime. For a window like `RANGE BETWEEN INTERVAL '2 HOURS' PRECEDING AND INTERVAL '2 HOURS' FOLLOWING` with a non uniform distribution of the partition with respect to the order by part (e.g. many sales happening in the morning), it might result in many rows getting added and dropped when the window shifts. Nevertheless,  each row is only added and removed once from the window frame aggregation.

For `MIN` and `MAX` aggregates it's necessary to maintain an auxiliary ordered search tree or heap of the entries in the window. Elements are added and removed as from this data structure as needed, resulting in an additional log factor in the runtime complexity. The SQL specification mandates that the bounds for the frame are constants, meaning that the above approaches work reasonably well.

For the lols, let us discard that constraint and assume windows of the form
`SUM(b) OVER (ORDER BY a ROWS BETWEEN x PRECEDING AND y FOLLOWING` are allowed. When the frame shifts the aggregate can then change arbitrarily, meaning that the removable cumulative approach from the previous section will no longer help. To avoid reverting to a naive approach, segment trees come to the rescue. A segment tree stores the aggregate for a set of subranges and let's you query the aggregate for any range in logarithmic time.

![sum_aggregate_segment](assets/sum_aggregate_segment.excalidraw.png){: .center-image }

In the example above the aggregated sum for frames 1 and 2 can be calculated by summing the blue and red circled nodes respectively. When calculating an aggregate with the segment tree one can either traverse the tree bottom-up, starting with the frame bounds, or top-down if the tree nodes also contain their respective range attached.

The segment tree approach allows to calculate arbitrary changing frames in $O(N \log N)$.  It should be noted that the additional overhead of constructing and traversing the tree might be costly in practice compared to the simpler cumulative approach even if theoretically the segment tree is faster.

We have illustrated the technique with the `SUM` aggregate, but the approaches can be adapted to work for `AVG` and `STDDEV` by storing more information in the nodes. It is worth noting that a different strategy can be chosen based on the size or statistics of a partition. Once the segment tree has been build it's straightforward to parallelize the work.

There are further optimizations that can be applied to window functions, particularly around the evaluation of multiple window functions. For instance, if one window function is over `PARTITION BY a, b` and another is over `PARTITION BY a, c`, you only need to calculate the partitioning for `a` once.

There a further optimisations one can adapt for window functions, mainly around the evaluation of multiple window functions. For instance, if one window function is over `PARTITION BY a, b` and another is over `PARTITION BY a, c`, you only need to calculate the partitioning for `a` once.

We recently added window functions to [XTDB](https://github.com/xtdb/xtdb), so maybe come by and say [hi](https://docs.xtdb.com/index.html).

#### References
[Efficient Processing of Window Functions in Analytical SQL Queries, Leis et al.](https://vldb.org/pvldb/vol8/p1058-leis.pdf)*
