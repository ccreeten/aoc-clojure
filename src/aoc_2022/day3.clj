(ns aoc-2022.day3
  (:require
    [util.collection :refer :all]
    [util.input :refer :all]
    [util.solution :refer :all]))

(defn to-item-priority [item]
  (let [ascii (int item)]
    (if (<= ascii 90)
      (- ascii 38)
      (- ascii 96))))

(defn to-rucksack-priority [[front back]]
  (first (filter (set front) back)))

(defn to-group-priority [rucksack-group]
  (let [[rs-1 rs-2 rs-3] (map #(map to-item-priority %) rucksack-group)]
    (first (filter (set rs-1) (filter (set rs-2) rs-3)))))

(defn part-1 [input]
  (->> (map #(map to-item-priority %) input)
       (map split-in-half)
       (map to-rucksack-priority)
       (reduce +)))

(defn part-2 [input]
  (->> (partition 3 input)
       (map to-group-priority)
       (reduce +)))

(defn -main [] (aoc-solve part-1 part-2 (input-lines "2022/day3.txt")))

; part 1: 7875
; part 2: 2479