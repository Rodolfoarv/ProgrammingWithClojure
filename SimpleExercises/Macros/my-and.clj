(defmacro my-and
  "Evaluates all its arguments one at a time, from left to
  right. If a form returns logical false (nil or false),
  my-and returns that value and doesn't evaluate any of the
  other expressions, otherwise it returns the value of the
  last expr. (my-and) returns true."
  ([] true)
  ([x] x)
  ([x & y]
   `(let [temp# ~x]
      (if temp#
          (my-and ~@y)
          temp#))))
