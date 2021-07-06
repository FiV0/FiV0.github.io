---
layout: post
title: Vaccinations and Ring Buffers
comments: true
redirect_from: "/2021/07/06/vaccinations_and_ring_buffers/"
permalink: vaccinations-and-ring-buffers
---

Mid-June I had my first Covid vaccine shot in one of Berlin's vaccination centers. The experience was rather pleasant
and really well organized. Everybody at the center was super enthusiastic, helpful and some even wished
me good luck. To a certain extent the center seemed a bit overstaffed, as at every corner somebody would tell
you where to go or sit down, but better this way then a completely disorganized vaccination campaign.

The process at the center can broadly be described in the following steps:
- waiting
- personal information checking
- waiting
- actual vaccination
- waiting (for possible immediate side effects like an allergic reaction)

The best parts and the ones that will get programmers the most excited were the first 2 waiting periods.
Imagine a circular chair arrangement (the chairs were not actually arranged in a cycle, but the
concept remains the same). A person arrives and you place him/her at some random chair in the cycle. The
next person arrives and you place him/her to the right of the first person and so on ... When a person
has to leave for the next stage of the process, you just take out the first person you
placed in the cycle of chairs, thereby growing the hole of empty chairs between the last and the first person who arrived
in the buffer, eh cycle of chairs. They were essentially using, as computer scientists would call it,
a [ring buffer](https://en.wikipedia.org/wiki/Circular_buffer) for people. The nice bit about this
procedure is that you never have to make people change chairs or move while waiting.

Once you were vaccinated you got placed in one of 6 rows, which where numbered 0 to 50 in increments
of 10. The increments indicated when you could leave in the ongoing or next hour. Vaccinated people
might have to wait a different amount of time based on whether having received their first or second shot.
Here the CS analogy is less obvious, but it could be seen as a sort of streamed
[bucket sort](https://en.wikipedia.org/wiki/Bucket_sort) with scheduled flushes of buckets.

A friend of mine told me afterwards that "it wasn't surprising that me, the _proceduralo_,
would love the organization" at the center. While leaving I was thinking that this country
can still do some stuff really great and efficiently. I was definitely hyped for the rest of the day.
