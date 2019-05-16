---
layout: post
title: Multiple value let in Common Lisp
comments: true
redirect_from: "/2019/05/16/mvlet/"

permalink: mvlet
---

I recently wrote some code where I often repeated the following pattern.
```cl
(multiple-value-bind (var1 var2)
  (funcall ...)
  (multiple-value-bind (var3 var4)
    (funcall ... ))
```
First of all the code is drifting to the right and secondly things get hard to read as the `multiple-value-bind` is not conveying much information. It would be nicer to write something like
```cl
(mvlet (((var1 var2) (funcall ...))
        ((var2 var4) (funcall ...))
        ...
        )
  ...)
```
Today's post concerns the above macro and the `let*` version of it. Let us first look at what I came up with for `mvlet*` as it is a lot simpler compared to `mvlet`.
```cl
(defmacro mvlet* ((&rest bindings) &body body)
  (mvlet*-helper bindings body))

(defun mvlet*-helper (bindings body)
  (let* ((binding (car bindings))
         (var (car binding))
         (form (cadr binding))
         (rec (if (endp (cdr bindings))
                  body
                  (list (mvlet*-helper (cdr bindings) body)))))
    (if (consp var)
        `(multiple-value-bind ,var ,form
           ,@rec)
        `(let (,binding)
           ,@rec))))
```
In `mvlet*` we just pass the list of bindings and the body to `mvlet*-helper` where we then process each binding recursively. The `mvlet*-helper` function could probably be written more  efficiently with tail recursion, but as the code gets expanded before compiling we don't really care. We have two cases when processing a binding of the form `(var form)`. Either the `var` part is a list, in which case we have to use a `multiple-value-bind` in the expansion or it's just a single variable where we place the binding lonely in a let. As a side node, one could have accumulated subsequent single variables and placed them in a single `let*`. The recursive call builds the expansion for the remaining bindings. The awkward looking `(list (mvlet*-helper...` is only there so we can use the splicing for the `body` and don't have to wrap it in a `progn`.

One thing that I realized aferwards is that `(declare (ignore ...))` statments won't work most of the time as these have to come immediatly after the bindings of `let`/`multiple-value-bind`'s.

Here is the code for the more involved `mvlet:

```cl
(defmacro mvlet ((&rest bindings) &body body)
  (mvlet-helper bindings body))

(defun split-bindings (bindings)
  (labels ((rec (rest acc)
             (if (endp rest)
                 (list (nreverse (car acc))
                       (nreverse (cadr acc)))
                 (rec (cdr rest)
                      (list (cons (caar rest) (car acc))
                            (cons (cadar rest) (cadr acc)))))))
    (rec bindings (list '() '()))))

(defun mk-gensym-list (lst)
  (mapcar #'(lambda (x)
              (declare (ignore x))
              (gensym))
          lst))

(defun mvlet-helper (bindings body)
  (let* ((vars-forms (split-bindings bindings))
         (vars (car vars-forms))
         (forms (cadr vars-forms))
         (var-gensyms (mapcar #'(lambda (var)
                                  (if (consp var)
                                      (mk-gensym-list var)
                                      (gensym)))
                              vars)))
    (labels ((helper (local-var-gensyms forms)
               ;; end case
               (cond ((endp local-var-gensyms)
                      `(let ,(mapcar #'(lambda (var gensym)
                                         (list var gensym))
                                     (flatten vars)
                                     (flatten var-gensyms))
                         ,@body))
                     ;; mv case
                     ((consp (car local-var-gensyms))
                      `(multiple-value-bind ,(car local-var-gensyms)
                         ,(car forms)
                         ,(helper (cdr local-var-gensyms)
                                  (cdr forms))))
                     ;; simple case
                     (t
                      `(let ((,(car local-var-gensyms)
                              ,(car forms)))
                         ,(helper (cdr local-var-gensyms)
                                  (cdr forms)))))))
    (helper var-gensyms forms))))
```
There are two helper functions `split-bindings` and `mk-gensym-list`. `split-bindings` just splits the list of bindings into a list of

I bundled the two macros, plus the macros `mvpsetq`, `mvdo` and `mvdo*` from Paul Graham's book [on lisp](http://www.paulgraham.com/onlisp.html) (I highly recommend this book if you want to really understand what macros are all about) into a small [library](https://github.com/FiV0/cl-mv). I realized only afterwards that people had already written a lot better and more sophisticated versions of the same idea. [Here](http://www.ai.sri.com/~stickel/mvlet.lisp) somebody wrote a lot more sophisticated version where you can specify if the return value is a `list` or comes as `values`. Secondly there is the liberary [metabang-bind](https://common-lisp.net/project/metabang-bind/user-guide.html) which combines even more than just `let` and `multiple-value-bind` into a single form.


