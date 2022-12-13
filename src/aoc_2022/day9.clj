(ns aoc-2022.day9
  (:require
    [clojure.set :refer :all]
    [clojure.string :refer [split]]
    [util.conversion :refer :all]
    [util.math :refer :all]
    [util.input :refer :all]
    [util.solution :refer :all]))

(defrecord motion [direction count])
(defrecord rope [parts history])

(defn parse-motion [line]
  (let [[direction count] (split line #" ")]
    (->motion direction (parse-int count))))

(defn to-delta [x]
  (if (= x 0) 0 (/ x (abs x))))

(defn touching [x y]
  (every? (partial >= 1) (map abs (map - x y))))

(defn move [[x y] direction]
  (case direction
    "U" [x (dec y)]
    "D" [x (inc y)]
    "L" [(dec x) y]
    "R" [(inc x) y]))

(defn move-part [front back]
  (cond->> back
           (not (touching front back)) (mapv + (mapv (comp to-delta -) front back))))

(defn move-rope-step [{:keys [parts history]} direction]
  (let [follow #(conj %1 (move-part (first %1) %2))
        head    (list (move (first parts) direction))
        moved   (reduce follow head (rest parts))]
    (->rope (reverse moved) (conj history (first moved)))))

(defn move-rope [rope {:keys [direction count]}]
  (reduce #(move-rope-step %1 %2) rope (repeat count direction)))

(defn simulate [motions length]
  (reduce move-rope (->rope (repeat length [0 0]) #{[0 0]}) motions))

(defn part-1 [input]
  (count (-> (simulate input 2) :history)))

(defn part-2 [input]
  (count (-> (simulate input 10) :history)))

(defn -main [] (aoc-solve part-1 part-2 (map parse-motion (input-lines "2022/day9.txt"))))

; part 1 : 6266
; part 2 : 2369