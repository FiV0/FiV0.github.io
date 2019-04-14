---
layout: post
title: Memoization in Common Lisp
comments: true
redirect_from: "/2019/04/14/memoization/"

permalink: memoization
---

I have decided to blog some more again. It will force me to properly learn the stuff I am writing about. As I am currently working with Common Lisp and I am still on some learning curve, I thought I blog about this interesting language.

Today I write a little about memoization.
How I would have done it and a more lispy way.
Memoization is a method where a function is only evaluated once on some specific input and the result is otherwise looked up in some hash table where it was stored on a previous computation.
The first thing that comes to mind, is to just wrap the whole function in some scope where a local hash table is keeping track of previous passed arguments plus their corresponding results. Here is the outcome of such an approach:
```cl
(defmacro define-memo-function (fn args &body body)
  (alexandria:with-gensyms (table val found)
   `(let ((,table (make-hash-table :test #'equal)))
      (defun ,fn ,args
        (multiple-value-bind (,val ,found)
          (gethash (list ,@args) ,table)
          (if ,found ,val
              (setf (gethash (list ,@args) ,table)
                    (apply #'(lambda ,args ,@body) (list ,@args)))))))))
```
Our favorite function for the fibonacci sequence would then be defined as:
```cl
(define-memo-function fibonacci (n)
  (if (<= n 1) n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
```
The gensyms on line 2 are there so we are not shadowning any global variables in the function's body.
We then define the hash table that will serve for remembering already computed results.
Notice the `#'equal` for testing equality as we want to hash the list of arguments.
We then start to define our function as usual but first checking that we have not computed the result before.
I started to write the macro to around the `if` on line 7 and then only realized that I can't just call the function I am currently defining again. We would then endlessly be looking for our arguments list in the hash table.
Therefore the rather weird looking `(apply #'(lambda ...`, where we are evaluating the body of the function.

This is a rather crude version of memoization done in Lisp. Some shortcomings are for example that one can not define the equality test for checking which arguments already have been computed. There is no way to clear the hashed results without redefining the function.
The following [code](http://norvig.com/paip/auxfns.lisp) is from Peter Norvig's "Principles of Artificial Intelligence Programming" and it solves all these issues and actually does the whole process in a lot lispier way, namely using symbols.
```cl
(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))
```
Our most beloved function would then be defined as follows:
```cl
(defun-memo fib (n)
  (if (<= n 1) n
      (+ (fib (- n 1)) (fib (- n 2)))))
```
The function gets defined as usual in `defun-memo` but then passed to `memoize` which first clears the hash table for the just defined function and then sets the functions symbol to a newley created closure defined in `memo`, the heart of the little program above.
The `memo` function first does similar checking to my version with the difference being that one can actually decide on which arguments of the function one wants to hash on and which equality testing function to use.
The other difference is that `memo` does not have to worry about creating some recursion issue because the function, without any memoization, has already been defined (see `defun-memo`) and can therefore be called as usual. The assignement of the memoized version to the fucntion symbol only happens after `memo` returns.
