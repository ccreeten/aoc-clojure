(ns aoc-2022.day14
  (:require
    [clojure.set :refer [difference]]
    [util.conversion :refer :all]
    [util.grid :refer :all]
    [util.input :refer :all]
    [util.solution :refer :all]))

(defn parse-wall [line]
  (->> (re-seq #"\d+" line)
       (map parse-int)
       (reverse)
       (partition 2)
       (partition 2 1)
       (mapcat #(apply range-inc-2d %))))

(defn height [blocked]
  (apply max (map first blocked)))

(defn drop-sand-1 [height blocked]
  (loop [[row col :as sand] [0 500]]
    (cond (> row height) nil
          (not (blocked [(inc row) col]))       (recur [(inc row) col])
          (not (blocked [(inc row) (dec col)])) (recur [(inc row) (dec col)])
          (not (blocked [(inc row) (inc col)])) (recur [(inc row) (inc col)])
          :else sand)))

; ctrl-c, ctrl-v...
(defn drop-sand-2 [height blocked]
  (loop [[row col :as sand] [0 500]]
    (cond (= row (inc height)) sand
          (not (blocked [(inc row) col]))       (recur [(inc row) col])
          (not (blocked [(inc row) (dec col)])) (recur [(inc row) (dec col)])
          (not (blocked [(inc row) (inc col)])) (recur [(inc row) (inc col)])
          (> row 0) sand
          :else nil)))

(defn fill [bricks drop-sand]
  (let [height (height bricks)]
    (loop [blocked bricks]
      (let [result (drop-sand height blocked)]
        (if (nil? result)
          blocked
          (recur (conj blocked result)))))))

(defn part-1 [input]
  (-> (fill input drop-sand-1)
      (difference input)
      (count)))

(defn part-2 [input]
  (-> (fill input drop-sand-2)
      (difference input)
      (count)
      (inc)))

(defn -main []
  (aoc-solve part-1 part-2
             (->> (input-lines "2022/day14.txt")
                  (map parse-wall)
                  (tree-seq seq? seq)
                  (filter vector?)
                  (into #{}))))

; part 1 : 961
; part 2 : 26375