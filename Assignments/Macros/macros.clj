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
