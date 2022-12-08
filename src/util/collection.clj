(ns util.collection)

(defn split-in-half [coll]
  (split-at (/ (count coll) 2) coll))

(defn init [vec]
  (subvec vec 0 (dec (count vec))))

(defn update-last [vec f]
  (conj (init vec) (f (last vec))))