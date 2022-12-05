(ns aoc-2022.day1
  (:require
    [clojure.string :refer [split]]
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.math :refer :all]))

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

(def input (input-split-by "2022/day1.txt" #"\n\n"))

(defn -main []
  (println "; part 1:" (part-1 input))
  (println "; part 2:" (part-2 input)))

; part 1: 68442
; part 2: 204837