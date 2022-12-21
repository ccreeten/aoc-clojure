(ns util.grid
  (:require
    [util.math :refer [range-inc]]))

(defn positions-2d [grid]
  (let [width (count (first grid)) height (count grid)]
    (for [row (range 0 height) col (range 0 width)]
      [row col])))

(defn grid-get [grid pos]
  (get-in grid pos))

(defn exists-2d? [grid [row col]]
  (let [width (count (first grid)) height (count grid)]
    (and (>= row 0) (>= col 0) (< row height) (< col width))))

(defn range-inc-2d [[from-row from-col] [to-row to-col]]
  (for [row (range-inc from-row to-row (if (< from-row to-row) 1 -1))
        col (range-inc from-col to-col (if (< from-col to-col) 1 -1))]
    [row col]))