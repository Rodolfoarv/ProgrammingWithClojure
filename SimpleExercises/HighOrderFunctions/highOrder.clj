(defn my-reduce
  "Own implementation of the reduce function. "
  [fun init lst]
  (if (empty? lst)
    init
    (fun (first lst) (my-reduce fun init (rest lst)))))

(defn compose
  "Compose f and g "
  [f g]
  (fn [x]
    (f (g x))))

(defn f1 [x] (* x x x))
(defn f2 [x] (+ x 2))
(def f3 (compose f1 f2))
(def f4 (compose f2 f1))
(def f5 (compose f3 f4))
