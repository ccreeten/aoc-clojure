(ns aoc-2022.day12
  (:require
    [clojure.set :refer [union]]
    [util.grid :refer :all]
    [util.input :refer :all]
    [util.solution :refer :all]))

(def deltas [[-1 0] [1 0] [0 -1] [0 1]])

(defn neighbours [grid cell]
  (->> deltas
       (map (partial map +) (repeat cell))
       (filter #(exists-2d? grid %))
       (filter #(<= (- (int (grid-get grid %)) (int (grid-get grid cell))) 1))))

(defn expand [grid cells seen]
  (->> cells
       (mapcat #(neighbours grid %))
       (filter #(not (seen %)))
       (set)))

(defn shortest-path [grid from to]
  (loop [from (set from) to to seen from depth 0]
    (if (some #(= % to) from)
      depth
      (let [expanded (expand grid from seen)]
        (recur expanded to (union seen (set expanded)) (inc depth))))))

(defn pos-of [grid value]
  (filter #(= (grid-get grid %) value) (positions-2d grid)))

(defn solve [input starting-pos]
  (let [s-pos (first (pos-of input \S))
        e-pos (first (pos-of input \E))
        grid  (assoc-in (assoc-in input e-pos \z) s-pos \a)]
    (shortest-path grid (starting-pos grid s-pos) e-pos)))

(defn part-1 [input]
  (solve input (fn [_ s-pos] [s-pos])))

(defn part-2 [input]
  (solve input (fn [grid _] (pos-of grid \a))))

(defn -main [] (aoc-solve part-1 part-2 (input-grid "2022/day12.txt")))

; part 1 : 391
; part 2 : 386