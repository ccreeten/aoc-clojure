(ns aoc-2022.day4
  (:require
    [clojure.string :refer [split]]
    [util.conversion :refer :all]
    [util.input :refer :all]))

(defrecord assignment [from to])

(defn parse-assignment [line]
  (apply ->assignment (map parse-int (split line #"-"))))

(defn parse-assignment-pair [line]
  (map parse-assignment (split line #",")))

(defn fully-contains [a1 a2]
  (and (<= (:from a1) (:from a2)) (>= (:to a1) (:to a2))))

(defn partially-contains [a1 a2]
  (or (and (<= (:from a1) (:from a2)) (>= (:to a1) (:from a2)))
      (and (<= (:from a1) (:to a2)) (>= (:to a1) (:to a2)))))

(defn part-1 [input]
  (->> (map parse-assignment-pair input)
       (filter #(or (fully-contains (first %) (second %)) (fully-contains (second %) (first %))))
       (count)))

(defn part-2 [input]
  (->> (map parse-assignment-pair input)
       (filter #(or (partially-contains (first %) (second %)) (partially-contains (second %) (first %))))
       (count)))

(def input (input-lines "2022/day4.txt"))

(defn -main []
  (println "; part 1:" (part-1 input))
  (println "; part 2:" (part-2 input)))

; part 1: 651
; part 2: 956