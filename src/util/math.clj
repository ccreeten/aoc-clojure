(ns util.math)

(defn sum [coll]
  (reduce + coll))

(defn abs [n]
  (max n (- n)))

(defn range-inc [start end step]
  (range start (if (< step 0) (dec end) (inc end)) step))

(defn sum-to [n]
  (/ (* n (inc n)) 2))

(defn sum-range [l r]
  (- (sum-to r) (sum-to (dec l))))