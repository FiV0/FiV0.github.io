---
layout: post
title: Lisp Macros
comments: false
redirect_from: "/2017/03/24/lisp_macros/"
permalink: lisp-macros
---

### Introduction ###

Lisp (and its dialects) permit code-transformations before compiling your program. These transforamtions are called macros and permit powerful
language abstractions that are to my knowledge not present in any other language. I know this sounds all very abstract so I best give an example.
Suppose you have some binary operator like `+` which you want to turn nary so that instead of

{% highlight lisp %}
  (+ 1 (+ 2 (+ 3 4)))
{% endhighlight %}

you can write

{% highlight lisp %}
  (nary+ 1 2 3 4)
{% endhighlight %}

I am well aware of the fact that `+` is already nary in Lisp. This example serves to illustrate the point.

### An involved example ###

{% highlight lisp %}
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(with-gensyms (,@gensyms)
       `(let (,,@(loop for g in gensyms
                       for n in names
                       collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names
                         for g in gensyms
                         collect `(,n ,g)))
             ,@body)))))
{% endhighlight %}
