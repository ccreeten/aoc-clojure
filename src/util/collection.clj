(ns util.collection)

(defn split-in-half [coll]
  (split-at (/ (count coll) 2) coll))
