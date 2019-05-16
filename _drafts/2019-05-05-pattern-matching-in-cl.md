---
layout: post
title: Pattern Matching in Common Lisp
comments: true
redirect_from: "/2019/05/05/pattern_matching/"
permalink: pattern_matching
---


Today I will write about pattern matching in common lisp. The goal is to write a pattern matching macro facility that lets us write the following:
```cl
(match pat
  (((a b) (+ a b))
   (#(a b) (+ a b))
   (c (error 'anything-matches))))
```
We want
