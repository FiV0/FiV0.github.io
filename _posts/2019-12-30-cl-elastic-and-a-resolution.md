---
layout: post
title: cl-elastic and a resolution 
comments: true
redirect_from: "/2019/11/28/cl-elastic-and-a-resolution/"
permalink: elasticsearch-wrapper-in-cl 
---

I wrote a small elasticsearch wrapper called [cl-elastic](https://github.com/FiV0/cl-elastic)
for Common Lisp. Mainly inspired by 
[spandex](https://github.com/mpenet/spandex), the idea was to keep it very simple. No DSL on top of another 
DSL, but just a simple REST client wrapper. Elasticsearch also seems to be changing their interface 
every now and then and that is just annoying when you want to keep up to date with a DSL interface. 
There are some other clients for elasticsearch that take the DSL approach 
([clesc](https://github.com/own-pt/clesc), 
[cl-elasticsearch](https://github.com/kraison/cl-elasticsearch),
[eclastic](https://github.com/gschjetne/eclastic)).

Here is how you would load the library and create a new index:
```cl
(ql:quicklisp :cl-elastic)
(use :cl-elastic)

(defvar *client* (make-instance '<client> :endpoint "http://localhost:9200"))

(send-request *client* '("elasticsearch-test") :method :put)
```
I won't go into all the details (although there aren't many) of the wrapper here, if you are 
interested check out the README of the repository.

One interesting feature I hadn't yet really used in Common Lisp are reader macros. A very good
introduction to them can be found in following [gist](https://gist.github.com/chaitanyagupta/9324402).
Where normal macros do their transformations at compile-time, reader macros do their 
transformations at read-time. They enable you to create new syntax for Lisp.

As JSON is the data format to interact with elasticsearch and we use
[yason](https://github.com/phmarek/yason) to transition between Common Lisp hashtables and JSON,
the library exports a simple reader syntax for literal hashmap construction.
```cl
(defvar foo "bar")
#{foo 1 "foo" 2}
;; => #<HASH-TABLE :TEST EQUAL :COUNT 2 {1005643E03}> 
```

The liberary also prints hashtables a little different, so don't be suprised if you 
don't see the exact output as above. The way reader macros work is 
that a functions get _dispatched_ when certain characters are read. So in our 
case this would be something like:
```cl
(set-dispatch-macro-character #\# #\{ #'|#{-reader-}|)
``` 
In this case, when the character sequence `#{` is read, the reader function `#{-reader-}` gets 
called (and yes in Lisp a function name can contain symbols like `#`, `|` and `{` ). 
There is also different function called `set-macro-character` which does the same thing but 
only accepts a single _special_ character (this is for example used for reading lists `( ... )`).
The corresponding reader function is as follows:

```cl
(defun |#{-reader-}| (stream char arg)
  (declare (ignore char arg))
  (let ((*readtable* (copy-readtable *readtable* nil)))
    (set-macro-character #\} (get-macro-character #\)))
    (let ((contents (read-delimited-list #\} stream t)))
      (let ((pairs (if contents 
                       (loop for pairs = contents then (cddr pairs)
                          collect (list (car pairs) (cadr pairs))
                          while (cddr pairs))
                       '()))
            (res (gensym)))
        `(let ((,res (make-hash-table :test #'equal)))
           ,@(mapcar
              (lambda (pair)
                `(setf (gethash ,(car pair) ,res) ,(cadr pair)))
              pairs)
           ,res)))))
```
Just ignore the second and third argument. `|#{-reader-}|` gets called
after `#{` has been read from `stream`. The function then copies the current
readtable to not mess up the reading of other objects outside of the scope
of the `#{ ... }` syntax. The "contents" of the hashtable are then read 
as a list into `contents` and subsequently paired up.
The expression `(set-macro-character #\} (get-macro-character #\)))` gives
the character `}` the same macro function as `)`. This is necessary as 
otherwise the last element in the expression `#{a 1}` would be read as `1}`.
The final part of the function creates a new hashtable with the corresponding
key/value pairs via syntax quoting.

The package [named-readtables](https://github.com/melisgl/named-readtables) is 
the current standard way to enable and disable reader syntax in Common Lisp.
It takes care of updating the variable `*readtable*` accordingly. To register a new 
readtable for the above syntax one would do:

```cl
(defreadtable hashtable-syntax
  (:merge :standard)
  (:macro-char #\# :dispatch)
  (:dispatch-macro-char #\# #\{ #'|#{-reader-}))
```
It is then as simple as
```cl
(in-readtable hashtable-syntax)
(in-readtable :standard)
```
to enable and disable the new reader syntax.

Last but not least, this is probably my last post of the year and completely
unrelated to the things above. I have decided to try to write one post
per week in 2020. This is therefore my pledge/resolution to write around 52 posts
next year. I will try something similar to 
[the morning paper](https://blog.acolyer.org/) by reviewing CS research papers.
If you haven't checked out that blog, I highly recommend it.
From what I remember, reading research papers will be quite a lot of work if 
you want to get into the nitty gritty details of every paper, especially if one
is not farmiliar with the topic. So it will probably happen that there will be some
posts that don't go as deep into the material as they should. Nevertheless 
this is in my opinion the best way to learn stuff for the longterm. By forcing
myself to write about the topics there is no way to just skim the papers.
