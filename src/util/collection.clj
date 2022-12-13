(ns util.collection)

(defn split-in-half [coll]
  (split-at (/ (count coll) 2) coll))

(defn firstv [vec]
  (subvec vec 0 (dec (count vec))))

(defn restv [vec]
  (if (empty? vec) [] (subvec vec 1 (count vec))))

(defn update-last [vec f]
  (conj (firstv vec) (f (last vec))))

(defn sortv [comp vec]
  (into [] (sort comp vec)))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn concatv [x y]
  (vec (concat x y)))