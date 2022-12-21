(ns aoc-2022.day18
  (:require
    [clojure.set :refer [union]]
    [util.collection :refer :all]
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.solution :refer :all]))

(def deltas [[0 0 -1] [0 0 1] [0 -1 0] [0 1 0] [-1 0 0] [1 0 0]])

(defn flood [cubes possible-pocket min-x max-x min-y max-y min-z max-z]
  (loop [[cur & more] (list possible-pocket) seen #{possible-pocket}]
    (cond (nil? cur) seen
          (or
            (<= (first cur) min-x)
            (>= (first cur) max-x)
            (<= (second cur) min-y)
            (>= (second cur) max-y)
            (<= (third cur) min-z)
            (>= (third cur) max-z)) nil
          :else (let [next (->> (map #(map + cur %) deltas)
                                (filter (complement seen))
                                (filter (complement cubes)))]
                  (recur (concat next more)
                         (conj seen cur))))))

(defn pocket-cubes [cubes]
  (let [min-x (apply min (map first cubes))
        max-x (apply max (map first cubes))
        min-y (apply min (map second cubes))
        max-y (apply max (map second cubes))
        min-z (apply min (map third cubes))
        max-z (apply max (map third cubes))
        bounds [min-x max-x min-y max-y min-z max-z]]
    (loop [[possible-pocket & more] (for [x (range (inc min-x) max-x) y (range (inc min-y) max-y) z (range (inc min-z) max-z)] [x y z])
           pocket-cubes #{}
           filled-cubes (set cubes)]
      (cond (nil? possible-pocket) pocket-cubes
            (or (pocket-cubes possible-pocket) (filled-cubes possible-pocket)) (recur more pocket-cubes filled-cubes)
            :else (let [result (apply flood filled-cubes possible-pocket bounds)]
                    (if (nil? result)
                      (recur more pocket-cubes filled-cubes)
                      (recur more (union pocket-cubes result) filled-cubes)))))))

(defn count-free-sides [cubes cube]
  (let [lookup (set cubes)]
    (->> (map #(map + cube %) deltas)
         (filter (complement lookup))
         (count))))

(defn part-1 [input]
  (reduce + (map #(count-free-sides input %) input)))

(defn part-2 [input]
  (- (part-1 input) (part-1 (pocket-cubes input))))

(defn -main [] (aoc-solve part-1 part-2 (map #(map parse-int (re-seq #"\d+" %)) (input-lines "2022/day18.txt"))))

; part 1 : 3530
; part 2 : 2000