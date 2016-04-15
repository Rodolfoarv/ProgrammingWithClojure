;;; ITESM CEM, April 14, 2016.
;;; Clojure Source File
;;; Activity: Macros
;;; Authors:
;;;          A01169701 Rodolfo Andrés Ramírez Valenzuela


(ns-name (create-ns 'macros))

;Write a macro called my-or that works as described above.
; Make sure that any expression gets evaluated at most once. You may not use Clojure's or macro. Usage examples:
(defmacro my-or
  "Evaluates its expressions one at a time, from left to right. If a form
  returns a logical true value, it returns that value and doesn't evaluate any of the other expressions,
  otherwise it returns the value of the last expression. (or) returns nil."
  ([] nil)
  ([x] x)
  ([x & y]
    `(let [temp# ~x]
        (if temp#
          temp#
          (my-or ~@y)))))

;Write a macro called do-loop that implements a post-test loop control statement.
; It must combine the functionality of C's do-while statement and Pascal's repeat-until statement.
(defn my-while
  [condition lst]
  `(while ~condition
      (do
        ~@lst)))

(defn my-until
  [condition lst]
  `(loop []
    (if-not ~condition
      (do
        ~@lst
        (recur))
        nil)))

(defmacro do-loop
  [x & y]
    (let [operation (first (last y)) condition (second (last y)) rest_arg (butlast y)]
      (if (= operation :while)
        (my-while condition (cons x rest_arg))
        (my-until condition (cons x rest_arg)))))

; Write a macro called def-pred, that takes a name, an arg vector, and a body of one
; or more expressions. The macro should define two predicate functions: a regular one
; and its negated version. The name of the negated predicate should be the same as name
; but with a "not-" prefix, and its result should be negated using the not function (see the test code for an example)

(defmacro def-pred
  [name args & predicates]
  (let [negated (symbol (str 'not- name))]
   `(do (defn ~name ~args ~@predicates)
        (defn ~negated ~args (not (do ~@predicates))))))


;Write a macro called defn-curry, that performs a currying transformation to a function definition.
; It takes as parameters a name, an args vector, and a body of one or more expressions.
; The macro should define a function called name that takes only the first argument from args and
; returns a function that takes the second argument from args and returns a function that takes the third argument
;  from args, and so on. The last function returned takes the last argument from args
;  and evaluates all the expressions in body using a do special form (see the test code for examples).

 (defmacro defn-curry
   ([name args & body]
    (let [parameter (first args) rest_arg (rest args)]
      (cond
        (= 0 (count args)) `(defn ~name []   (do ~@body))
        (= 1 (count args)) `(defn ~name [~parameter] (do ~@body))
        :else
          `(defn ~name [~parameter] (curry_aux ~body ~@rest_arg))))))

 (defmacro curry_aux
   ([body parameter]
    `(fn [~parameter] (do ~@body)))
   ([body parameter & rest]
    `(fn [~parameter] (curry_aux ~body ~@rest))))
