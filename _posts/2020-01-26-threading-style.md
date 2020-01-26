---
layout: post
title: Threading Style
comments: true
redirect_from: "/2020/01/26/threading-style/"
permalink: threading-style
---

I came across some really interesting [blog post](https://stuartsierra.com/2018/07/06/threading-with-style) 
concerning Clojures' threading macros. The post desribes some Do's and Don'ts of Clojure threading macros.
I encourage everybody reading this to read the post. For whose unaware of Clojures' threading macros 
`->` (thread-first) and `->>` (thread-last), they are used to solve, what I call, the Inward-Outward 
problem of Lisp. The Inward-Outward problem is the problem where one needs to read the inner functions 
applications before the outer ones to understand the context. I actually wrote a previous 
[post](fiv0.githout.io/clojure-arrows) on how to write these macros in Common Lisp. The typical example 
for the `->>` macro in Clojure is the transformation of a sequence.
```clj
(->> (range 1 10)
     (filter even?)
     (map inc)
     (reverse))
```
The author of the post actually argues that this is the __only__ way one should use the thread-last macro. Only use it
with sequence functions. His argument is that the value that is being *threaded* should stay in the same 
*shape*, except maybe for some collecting at the end, as it is otherwise very difficult for the reader to 
follow along. I must admit that I did not always follow this rule in some early code I wrote, but that I agree
with the statement. Sometimes when I come back to one of those code snippets where I violated the rule
it takes me a lot of time to actually figure out what is going on.

The thread-first macro `->`, according to the author, should only be used with navigating nested structures 
and the transformation of a value. One big NoNo is to mix `->` and `->>`. I don't think I ever committed this 
heresy, but I commited another one which the author 
[warns against](https://stuartsierra.com/2018/07/15/clojure-donts-thread-as), 
namely the standalone use of `as->` (thread-as). Once in while, you come across a problem where everything 
fits neatly into a threading macro, but one of the functions you apply doesn't take the threaded value as 
first/last value. I thought in this case one should use thread-as. Apparently you are not! 
An example of what I have done before is:
```clj
(as-> (get-lazy-sequence-foo-from-api-call) foo-seq 
  (take (calculate-limit foo-seq) foo-seq)
  (map some-function foo-seq))
```
Here I am using `as->` because I want to condition on the value of the threaded value in the second line. The post
discourages the above (although it is not explicitly mentioned what is supposed to be done if you want to use 
the threaded value twice). I think one could just write an explicit function for the things calculated in the 
second line that takes the threaded sequence as last value. I personally think one could
use the `as->` alone if the sequence stays *the same* type/shape along the transformation and therefore a 
consistent name could be given. Maybe this happens to rarely as the transformations have some purpose.

Apparently `as->` should mainly be used inside a `->` macro. Namely when the transformation being applied
doesn't take the threaded value as first argument. Another interesting idea that I gathered from the clojurians
slack is that one could use `as->` for labeling the transformations, therefore giving more context to the reader.
An example (although rather redundent) could be:
```clj
(-> (get-accounts person ...) 
    (as-> accounts (get-savings-account accounts))
    (as-> savings-account (get-balance savings-account)))
```

Some code I have written which doesn't look very clojury looks as follows:
```clj
(let [violations (if (card-active? account) ["card-not-active"] [])
      violations (if (insufficient-limit? account transaction)
                   (conj violations "insufficient-limit") violations)
      ...])
```
I think this is a typical pattern for somebody coming from imperative programming. Clojure has a `cond->` macro
which solves this issue very elegently. The `cond->` macro works like the `->` macro except one can interleave 
a condition before every thread-first expression which is only executed if the condition is met and otherwise
the value from the previous threaded expression is past on.
```clj
(cond-> []
  (card-active? account) (conj "cart-not-active")
  (insufficient-limit? account transaction) (conj "insufficient-limit")
  ...)
```
