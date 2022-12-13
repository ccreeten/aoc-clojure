(ns aoc-2022.day6
  (:require
    [util.input :refer :all]
    [util.solution :refer :all]))

(defn find-marker [buffer length]
  (->> (partition length 1 buffer)
       (keep-indexed #(if (apply distinct? %2) %1))
       (first)
       (+ length)))

(defn part-1 [input]
  (find-marker input 4))

(defn part-2 [input]
  (find-marker input 14))

(defn -main [] (aoc-solve part-1 part-2 (input-all "2022/day6.txt")))

; part 1: 1876
; part 2: 2202