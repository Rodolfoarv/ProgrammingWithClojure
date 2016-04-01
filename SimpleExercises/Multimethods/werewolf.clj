(ns were-creatures)
 (defmulti full-moon-behavior (fn [were-creature] (:were-type were-creature)))
 (defmethod full-moon-behavior :wolf
  [were-creature]
  (str (:name were-creature) " will howl and murder"))
 (defmethod full-moon-behavior :simmons
  [were-creature]
  (str (:name were-creature) " will encourage people and sweat to the oldies"))



(full-moon-behavior {:name "Andy the baker"
                    :were-type :simmons})
; => "Andy the baker will encourage people and sweat to the oldies"

(full-moon-behavior {:were-type :wolf
                     :name "Rachel from next door"})
; => "Rachel from next door will howl and murder"
