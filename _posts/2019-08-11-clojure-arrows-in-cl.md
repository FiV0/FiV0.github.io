---
layout: post
title: Clojure arrows in Common Lisp
comments: true
redirect_from: "/2019/08/11/clojure-arrows-in-cl/"

permalink: clojure-arrows
---

Clojure has two nice little integrated macros `->` and `->>`, called the arrow macros. They are supposed to improve readibility.
As Clojure has Java interoperatibility, these macros are especially helpful when calling multiple methods on a java object.
Something like
```cl
(subseq (alexandria:flatten (loop for i from 0 to 3
                               collect (loop for j from 0 to 5
                                          collect j)))
        3 6)
```
can then be rewritten as
```cl
(-> (loop for i from 0 to 3
       collect (loop for j from 0 to 5
                  collect j))
    alexandria:flatten
    (subseq 3 6)) ;; --> (3 4 5)
```
The thread first macro `->` places the first expression as first argument in the second expression. The resulting expression
is then placed as second argument of the third expression and so on.
If the second expression is a list the first expression is just passed in as it is
(list case). If the
second expression is a symbol a new list is created with the second expression a first element and the first expression a second
element (symbol case). Ditto for the remaining expressions.
The thread last macro `->>` does the same thing, except that it places the previous
expression as last argument of subsequent calls. An example would be:
```cl
(->> (loop for i from 0 to 3
        collect (loop for j from 0 to 5
                   collect j))
     alexandria:flatten
     (apply #'+)) ;; --> 60
```
A possible implementation of the macros is as follows:
```cl
(defmacro -> (x &rest forms)
  "Thread first macro, like in clojure."
  (arrow-helper forms x))

(defmacro ->> (x &rest forms)
  "Thread last macro, like in clojure."
  (arrow-helper forms x T))

(defun arrow-helper (forms res &optional (end nil))
  (if forms
      (let ((first-form (car forms)))
        (if (listp first-form)
            ;; list case
            (arrow-helper (cdr forms)
                          ;; branching on whether to insert
                          ;; infront or at the end
                          (if end
                              (nconc first-form (list res))
                              (cons (car first-form)
                                    (cons res (cdr first-form))))
                          end)
            ;; simple symbol case
            (arrow-helper (cdr forms) (list first-form res) end)))
      res))
```
The `arrow-helper` function recursively expands all the forms and checks on the way if it is a list case or a symbol case.
It is also parameterized by an optional argument, so we can use it for `->` and `->>`.

Someone has already ported these macros to Common Lisp in a small library callled
[cl-arrows](https://github.com/nightfly19/cl-arrows)
and a more extensive one called [arrow-macros](https://github.com/hipeta/arrow-macros/).
