---
layout: post
title: Anonymous recursive function in Clojure 
comments: true
redirect_from: "/2019/10/23/anonymous-recursive-function-in-clojure/"
permalink: anon-rec-function
---

I was recently doing some problems on [4Clojure](http://www.4clojure.com/) (a site for learning clojure by doing problems). 
Mainly to get used to the syntax of Clojure. The problems are most of the time not really difficult, but just try to teach
you something about the core language features and how to apply them. One of the problems was to calculate the 
[levenshtein](https://en.wikipedia.org/wiki/Levenshtein_distance). 
As Clojure has a build-in [memoization](http://finnvolkel.com/memoization) function one can implement the straightforward 
recurrence using `memoize`.
```clj
(def levenshtein
  (memoize (fn [x y]
             (cond
               (empty? x) (count y)
               (empty? y) (count x)
               :else (min
                      (+ (if (= (.substring x 0 1) (.substring y 0 1)) 0 1)
                         (levenshtein (.substring x 1) (.substring y 1))) 
                      (inc (levenshtein x (.substring y 1)))
                      (inc (levenshtein (.substring x 1) y)))))))
```
I think this is some very clean code one can easily reason about. Maybe the memoization adds more overhead then a non-recursive version,
but in terms of simplicity this seems hard to beat. The problem is that 4Clojure doesn't allow `def` for security reasons. So I was 
wondering if one could write an anonymous recursive function while using the standard `memoize` function. Once you try this you
quickly realize that you somehow want to refer to the memoized function and it feels like you need some kind of `def` or `defmacro`.
I ask on the clojurians slack, as I didn't see any solution to the problem, and someone pointed me to 
[this](https://stackoverflow.com/questions/3906831/how-do-i-generate-memoized-recursive-functions-in-clojure/13123571#13123571) 
Stackoverflow question. The trick is to create an anonymous function that takes itself as first argument.
```clj
((let [levenshtein
       (fn [mem-fn x y]
         ;; redfining levenshtein using the memoized version as first argument
         (let [levenshtein (fn [x y]
                             (mem-fn mem-fn x y))]
           (cond (empty? x) (count y)
                 (empty? y) (count x)
                 :else (min
                        (+ (if (= (.substring x 0 1) (.substring y 0 1)) 0 1)
                           (levenshtein (.substring x 1) (.substring y 1)))
                        (inc (levenshtein x (.substring y 1)))
                        (inc (levenshtein (.substring x 1) y))))))
       mem-fn (memoize levenshtein)]
   (partial mem-fn mem-fn))
 "gaattctaatctc" "caaacaaaaaattt")
;; => 9
```
The *outer* `levenshtein` also takes its own memoized version as argument, inside its body we redefine `levenshtein` to make
use of the memoized version. Once the levenshtein is defined, we create a memoized version of it (`mem-fn`). The anonymous function
is then the partial application of the momoized version with itself as first argument. Kind of contrieved, I know. 
The author of the Stackoverflow anwser also mentions that this is similar to the 
[y-combinator](https://en.wikipedia.org/wiki/Fixed-point_combinator#Fixed_point_combinators_in_lambda_calculus) which achieves 
recursion without in-build recursion.

One can also write a little macro that allows the memoization of lambdas (courtesy of a clojurian).
```clj
(ns lambda)

(defmacro memoize [lambda]
  `(let [mem-fn# (memoize
                  (fn ~(vec (cons 'mem-fn (nth lambda 2)))
                    (let [~(nth lambda 1)
                          (fn ~(nth lambda 2)
                            ~(concat (list 'mem-fn 'mem-fn) (nth lambda 2)))]
                      ~@(drop 3 lambda))))]
     (partial mem-fn# mem-fn#)))
```
The macro does all of the transformations for you that we did explicitly above. One can then write the `levenshtein` function really 
simple again.
```clj
((lambda/memoize
  (fn lev [x y]
    (cond (empty? x) (count y)
          (empty? y) (count x)
          :else (min
                 (+ (if (= (.substring x 0 1) (.substring y 0 1)) 0 1)
                    (lev (.substring x 1) (.substring y 1))) 
                 (inc (lev x (.substring y 1)))
                 (inc (lev (.substring x 1) y))))))
 "gaattctaatctc" "caaacaaaaaattt")
```
This makes use of the standard Clojure lambda syntax where you can name lambdas.
