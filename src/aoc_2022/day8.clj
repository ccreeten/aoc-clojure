(ns aoc-2022.day8
  (:require
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.math :refer :all]
    [util.solution :refer :all]))

(defrecord forest [grid transpose])

(defn views [{:keys [grid transpose]} x y]
  (let [row (nth grid x) col (nth transpose y)]
    (list (reverse (take y row))
          (reverse (take x col))
          (drop (inc y) row)
          (drop (inc x) col))))

(defn eval-locations [map f]
  (let [grid (:grid map) range (range 0 (count (first grid)))]
    (for [x range y range] (f (get-in grid [x y]) (views map x y)))))

(defn visible? [val views]
  (some #(every? (partial > val) %) views))

(defn score-view [val view]
  (let [smaller (take-while #(> val %) view) distance (count smaller)]
    (cond-> distance (not= distance (count view)) inc)))

(defn scenic-score [val views]
  (reduce * (map #(score-view val %) views)))

(defn part-1 [input]
  (count (filter true? (eval-locations input visible?))))

(defn part-2 [input]
  (apply max (eval-locations input scenic-score)))

(defn -main [] (aoc-solve part-1 part-2 (let [grid (input-int-grid "2022/day8.txt")] (->forest grid (apply mapv vector grid)))))

; part 1: 1546
; part 2: 519064