(ns aoc-2022.day13
  (:require
    [util.collection :refer :all]
    [util.input :refer :all]
    [util.solution :refer :all]
    [util.string :refer :all])
  (:import (clojure.lang IPersistentVector)))

(derive Number ::num)
(derive IPersistentVector ::vec)

(defmulti order (fn [l r] [(type l) (type r)]))

(defmethod order [::num ::num] [l r] (if (= l r) nil (< l r)))
(defmethod order [::num ::vec] [l r] (order [l] r))
(defmethod order [::vec ::num] [l r] (order l [r]))
(defmethod order [::vec ::vec] [l r]
  (cond (or (seq l) (seq r))
        (first (filter some? (map #(%) [#(find-first boolean? (map order l r))
                                        #(order (count l) (count r))
                                        #(order (restv l) (restv r))])))))

(defn part-1 [input]
  (let [pairs (vec (partition 2 input))]
    (->> (range 0 (count pairs))
         (filter #(order (first (get pairs %)) (second (get pairs %))))
         (map inc)
         (apply +))))

(defn part-2 [input]
  (let [dividers #{[[2]] [[6]]}
        extended (concatv input (vec dividers))
        sorted   (sortv #(order %1 %2) extended)]
    (->> (range 0 (count sorted))
         (filter #(dividers (get sorted %)))
         (map inc)
         (apply *))))

(defn -main []
  (aoc-solve part-1 part-2 (mapv read-string (filter not-blank? (input-lines "2022/day13.txt")))))

; part 1 : 5760
; part 2 : 26670