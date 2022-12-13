(ns aoc-2022.day1
  (:require
    [clojure.string :refer [split]]
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.math :refer :all]
    [util.solution :refer :all]))

(defn part-1 [input]
  (->> (map #(map parse-int (split % #"\n")) input)
       (map sum)
       (reduce max)))

(defn part-2 [input]
  (->> (map #(map parse-int (split % #"\n")) input)
       (map sum)
       (sort >)
       (take 3)
       (reduce +)))

(defn -main [] (aoc-solve part-1 part-2 (input-split-by "2022/day1.txt" #"\n\n")))

; part 1: 68442
; part 2: 204837