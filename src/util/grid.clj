(ns util.grid)

(defn positions-2d [grid]
  (let [width (count (first grid)) height (count grid)]
    (for [row (range 0 height) col (range 0 width)]
      [row col])))

(defn grid-get [grid pos]
  (get-in grid pos))

(defn exists-2d? [grid [row col]]
  (let [width (count (first grid)) height (count grid)]
    (and (>= row 0) (>= col 0) (< row height) (< col width))))