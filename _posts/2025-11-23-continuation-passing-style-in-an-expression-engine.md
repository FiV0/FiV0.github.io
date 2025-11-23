---
layout: post
title: Continuation-passing style in an expression engine
comments: true
redirect_from: "/2025/11/23/continuation-passing-style-in-an-expression-engine/"
permalink: continuation-passing-style-in-an-expression-engine
---

Continuation Passing Style (CPS henceforth) is usually something from esoteric functional programming languages. I want to show you a little example of how it is used in the [XTDB](https://xtdb.com/) Expression Engine. Before we start, let's have a look at what CPS actually is. A continuation in a Lisp-flavoured language like Clojure is a function of the value of a subexpression to the value of the program (also an expression). So consider the expression[^1]
```clojure
(+ (* 1 2) (* 3 4))
```
When have evaluated `1` the continuation of the whole expression is
```clojure
(fn [v] (+ (* v 2) (* 3 4)))
```
So a continuation is a function itself (if the programming language supports functions). A general function transformed to CPS style, is a function that always takes an extra argument, the continuation. CPS style functions don't return a value but call the passed in continuation with their result value. So to get an actual value out of an CPS style function, you need to call it with `identity` as continuation.
```clojure
;; Standard naive factorial implementation
(defn fac [n]
  (if (zero? n)
    1
    (* n (fac (dec n)))))

(fac 5)
;; => 120

;; CPS style factorial implementation
(defn fac-cps [n cont]
  (if (zero? n)
    (cont 1)
    (fac-cps (dec n) (fn [res] (cont (* n res))))))

(fac-cps 5 identity)
;; => 120
``` 

You might wonder why you ever want to program like this. It seems extremely cumbersome. 
### A little language

Let's take a step back and focus on something quite different first to motivate our final example. Let's build a little expression engine. The language for our example is quite limited but will suffice to convey the ideas.
```clojure
;; null literals
nil
;; number literals 
1 
;; addition
(+ 1 2)
;; variables (just symbols)
x
;; conditionals / branching 
'(if condition if-branch else-branch)
; local bindings
'(let [binding expr] 
   body)
```
Every expression in this language either produces null or an integer. For semantics we follow [SQL's](https://en.wikipedia.org/wiki/Null_(SQL)#Comparisons_with_NULL_and_the_three-valued_logic_(3VL)) [three-valued logic](https://en.wikipedia.org/wiki/Three-valued_logic) approach. Hence any addition involving a `nil` results in `nil` and as we don't have `true` or `false` in the language anything other then `nil` is considered truthy.

### An interpreter
Let's start with a simple interpreter. First lets define a protocol that can be invoked on any parsed expression[^2].
```clojure
(defprotocol Expr
  (invoke [this env]))
```
The protocol is going to take the parsed expression and the environment containing any instantiation of variables.

```clojure
(defrecord NullExpr []
  Expr
  (invoke [_ _env] nil))

(defrecord LongExpr [lng]
  Expr
  (invoke [_ _env] lng))

(defrecord VarExpr [var]
  Expr
  (invoke [_ env]
    (if (contains? env var)
      (get env var)
      (throw (UnsupportedOperationException.)))))

(defrecord PlusExpr [args]
  Expr
  (invoke [_ env]
    (let [args (map #(invoke % env) args)]
      (if (some nil? args)
        nil
        (apply + args)))))

(defrecord IfExpr [cond if-branch else-branch]
  Expr
  (invoke [_ env]
    (if (invoke cond env)
      (invoke if-branch env)
      (invoke else-branch env))))

(defrecord LetExpr [binding b-expr body]
  Expr
  (invoke [_ env]
    (invoke body (assoc env binding (invoke b-expr env)))))
```
We are missing the parsing step.
```clojure
(defmulti parse-expr (fn [expr]
                       (cond (nil? expr) :nil
                             (number? expr) :long
                             (symbol? expr) :var
                             (list? expr) (keyword (first expr)))))

(defmethod parse-expr :nil [_] (->NullExpr))
(defmethod parse-expr :long [lng] (->LongExpr lng))
(defmethod parse-expr :var [var] (->VarExpr var))
(defmethod parse-expr :+ [[_ & args]]
  (->PlusExpr (map parse-expr args)))
(defmethod parse-expr :if [[_ cond if-branch else-branch]]
  (->IfExpr (parse-expr cond) (parse-expr if-branch) (parse-expr else-branch)))
(defmethod parse-expr :let [[_ [binding b-expr] body]]
  (->LetExpr binding (parse-expr b-expr) (parse-expr body)))
```

Putting it all together:
```clojure
(-> (parse-expr '(let [x (if (+ 1 y) 3 4)]
                     (+ 1 2 x)))
    (invoke {'y 1}))
```
If this approach is used as an expression engine for an SQL engine `y` won't be a single scalar but rather a vector for which the expression gets called on every element. The interpreter approach is therefore rather slow.
### A direct compiler
Let's first define some interfaces for accessing elements in our vectors.
```clojure
(defprotocol IVectorReader
  (^void isNull [this idx])
  (^long getLong [this idx]))

;; In this world we only consider append only vectors
(defprotocol IVectorWriter
  (^void writeNull [this])
  (^void writeLong [this lng]))
```
Let's generate standard Clojure expressions from an incoming expression in our tiny toy language.
```clojure
(defmulti codegen-direct (fn [expr] (cond (nil? expr) :nil
                                          (number? expr) :long
                                          (symbol? expr) :var
                                          (list? expr) (keyword (first expr)))))

(defmethod codegen-direct :nil [_] nil)
(defmethod codegen-direct :long [lng] lng)

;; The index we are accessing in all vector involved 
(def idx-sym (gensym 'idx))

(defmethod codegen-direct :var [var]
  `(when-not (.isNull ~var ~idx-sym)
     (.getLong ~var ~idx-sym)))

(defmethod codegen-direct :+ [[_ x-expr y-expr]]
  `(if-let [x-res# ~(codegen-direct x-expr)]
     (if-let [y-res# ~(codegen-direct y-expr)]
       (Math/addExact x-res# y-res#)
       nil)
     nil))

(defmethod codegen-direct :if [[_ cond if-branch else-branch]]
  `(if ~(codegen-direct cond)
     ~(codegen-direct if-branch)
     ~(codegen-direct else-branch)))

(defmethod codegen-direct :let [[_ [binding b-expr] body]]
  `(let [~binding (->value-box)]
     (if-let [b-expr-res# ~(codegen-direct b-expr)]
       (.writeLong ~binding b-expr-res#))
     ~(codegen-direct body)))
```
Finally we need to setup the vectors and the index we are currently accessing around the generated expression.
```clojure
(defn compile-expr [expr col-names]
  (-> `(fn ~(vec col-names)
         (let [res-vec# (->vec-wrt)]
           (dotimes [~idx-sym (count ~(first col-names))]
             (if-let [res# ~(codegen-direct expr)]
               (.writeLong res-vec# res#)
               (.writeNull res-vec#)))
           res-vec#))
      #_(doto clojure.pprint/pprint) ; for debugging
      eval))
```
Note: *`->value-box` and `->vec-wrt` return implementations of the reader and writer interfaces above. The details don't matter so much for this post except that `->value-box` is used for intermediate results and is purely a wrapper around a single value.*

The function takes an expression in the toy language and a set of vector readers and then applies the generated code for the expression for each vector index (This implicitly assumes all vectors are the same length). We can write something like `(if-let [res# ~(codegen-direct expr)] ...)` because we know the only two possible result types in our language are `nil` and `number`. What happens if the result contains more types? What if the handling in the code generation is dependent on the type of a subexpression?
### A CPS-style compiler
The continuation function of our `compile-expr` call won't only take the resulting generated code, but rather the return type as first argument and the generated code as second. 
```clojure
(defmulti codegen-expr (fn [expr _cont]
                         (cond (nil? expr) :nil
                               (number? expr) :long
                               (symbol? expr) :var
                               (list? expr) (keyword (first expr)))))

(defmethod codegen-expr :nil [_ cont] (cont :nil nil))
(defmethod codegen-expr :long [lng cont] (cont :long lng))
(defmethod codegen-expr :var [var cont]
  `(if (.isNull ~var ~idx-sym)
     ~(cont :nil nil)
     ~(cont :long `(.getLong ~var ~idx-sym))))

(defmethod codegen-expr :+ [[_ x-expr y-expr] cont]
  (codegen-expr x-expr
                (fn continue-x [x-type x-code]
                  (case x-type
                    :long (codegen-expr y-expr
                                        (fn continue-y [y-type y-code]
                                          (case y-type
                                            :long (cont :long `(Math/addExact ~x-code ~y-code))
                                            :nil (cont :nil nil))))
                    :nil (cont :nil nil)))))

(defmethod codegen-expr :if [[_ cond if-branch else-branch] cont]
  `(if ~(codegen-expr cond (fn [_ x] x))
     ~(codegen-expr if-branch cont)
     ~(codegen-expr else-branch cont)))

(defmethod codegen-expr :let [[_ [binding b-expr] body] cont]
  (codegen-expr b-expr
                (fn [b-type b-code]
                  `(let [~binding (->value-box)]
                     ~(case b-type
                        :nil
                        `(do
                           (.writeNull ~binding)
                           ~(codegen-expr body (fn [body-type body-code]
                                                 (cont body-type body-code))))
                        :long
                        `(do
                           (.writeLong ~binding ~b-code)
                           ~(codegen-expr body (fn [body-type body-code]
                                                 (cont body-type body-code)))))))))
```
Correspondingly to our direct compiler example we define a `compiler-expr2`.
```clojure
(defn compile-expr2 [expr col-names]
  (let [res-vec-sym (gensym 'res-vec)]
    (-> `(fn ~(vec col-names)
           (let [~res-vec-sym (->vec-wrt)]
             (dotimes [~idx-sym (count ~(first col-names))]
               ~(codegen-expr expr
                              (fn [out-type out-code]
                                (case out-type
                                  :nil `(.writeNull ~res-vec-sym)
                                  :long `(.writeLong ~res-vec-sym ~out-code)))))
             ~res-vec-sym))
        #_(doto clojure.pprint/pprint) ; for debugging
        eval)))
```
CPS helps to decide how (based on the type) to use the returned value (the code). For example look at the continuation passed in to the outermost `codegen-expr` call on line 7. 
```clojure
(fn [out-type out-code]
    (case out-type
       :nil `(.writeNull ~res-vec-sym)
       :long `(.writeLong ~res-vec-sym ~out-code)))
```
We can branch based on the type of the expression without ever evaluating the expression. This becomes more powerful as your type systems grows.

For example an operator like `+` doesn't need to know anything about the actual operands. The passed in result-type can be be used to decide how `+` should behave based on the first operand's type. This could be polymorphic type handling or natural short-circuiting opportunities. In the `+` CPY-style `codegen-expr` implementation above we immediately cut short in case the `x-type` is `nil`. The result doesn't need to know anything about how it will be used. This also makes the CPS-style approach quite easily extensible.

As XTDB handles union types. A direct compiler would need to do instance checks as the returned expression might be of an arbitrary type. The CPS style approach let's you do dynamic dispatch on the result type without complicated casting.

The CPS offers in my opinion better logical locality (hardware locality is a different beast). The `type` and`code` forces all the necessary logic for a decision to be available locally. In a direct approach you will likely need get all this information from some passed through context or a previous pass.

All that to say that there are also downsides to the CPS approach.
An initial direct compiler version might be easier to implement and debug. The inversion of control in CPS makes a bit harder to wrap your head around at first. 

[^1]: Example shamelessly stolen from [Control structures](https://xavierleroy.org/control-structures/book/main008.html) .

[^2]: If you want to run the whole REPL session yourself. You can find the code [here](https://github.com/FiV0/continuation-passing-style/blob/8f77d14c466d045f08326da1aa6c571536ffa32e/src/continuous_passing_style.clj)
