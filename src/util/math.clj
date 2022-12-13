(ns util.math)

(defn sum [coll]
  (reduce + coll))

(defn abs [n]
  (max n (- n)))