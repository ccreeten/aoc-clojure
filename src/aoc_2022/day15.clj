(ns aoc-2022.day15
  (:require
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.math :refer :all]
    [util.solution :refer :all]))

(defn distance [from to]
  (reduce + (map abs (map - from to))))

(defn parse-entry [line]
  (->> (re-seq #"-?\d+" line)
       (map parse-int)
       (partition 2)
       (map reverse)
       (apply #(vector %1 (distance %1 %2)))))

(defn view-distance [on-row [sensor total-range]]
  (let [[row col] sensor
        view-distance (- total-range (abs (- on-row row)))]
    (if (< view-distance 0)
      []
      [(- col view-distance) (+ col view-distance)])))

(defn merge-range [[fx tx] [fy ty]]
  [(min fx fy) (max tx ty)])

(defn merge-ranges [ranges]
  (let [sorted (sort #(compare (first %1) (first %2)) ranges)]
    (loop [[right & more] (rest sorted) merged [(first sorted)]]
      (if (nil? right)
        merged
        (let [left (last merged)]
          (if (>= (second left) (dec (first right)))
            (recur more (assoc merged (dec (count merged)) (merge-range left right)))
            (recur more (conj merged right))))))))

(defn row-coverage [sensors row]
  (->> (map (partial view-distance row) sensors)
       (filter not-empty)
       (merge-ranges)))

(defn undetected [sensors row]
  (let [ranges (row-coverage sensors row)]
    (if (<= (count ranges) 1)
      []
      [row (inc (second (first ranges)))])))

(defn part-1 [input]
  (->> (row-coverage input 2000000)
       (map #(apply - (reverse %)))
       (sum)))

(defn part-2 [input]
  (->> (range 0 (inc 4000000))
       (map #(undetected input %))
       (filter not-empty)
       (map #(+ (first %) (* (second %) 4000000)))
       (first)))

(defn -main [] (aoc-solve part-1 part-2 (map parse-entry (input-lines "2022/day15.txt"))))

; part 1 : 5083287
; part 2 : 13134039205729