(ns aoc-2022.day5
  (:require
    [clojure.string :refer [split]]
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.string :refer :all]
    [util.solution :refer :all]))

(defrecord move [count from to])

(defn parse-stacks [stacks]
  (->> (split stacks #"\n")
       (mapv #(re-seq #"[A-Z]|    " %))
       (apply mapv list)
       (mapv #(filter not-blank? %))))

(defn parse-move [move]
  (let [[count from to] (map parse-int (re-seq #"\d+" move))]
    (->move count (dec from) (dec to))))

(defn parse-moves [moves]
  (map parse-move (split moves #"\n")))

(defn parse-begin-state [input]
  (let [[stacks moves] (split input #"[ \d]+\n\n")]
    [(parse-stacks stacks) (parse-moves moves)]))

(defn apply-move [order stacks {:keys [count from to]}]
  (-> stacks
      (assoc from (drop count (nth stacks from)))
      (assoc to (concat (order (take count (nth stacks from))) (nth stacks to)))))

(defn part-1 [[stacks moves]]
  (->> (reduce (partial apply-move reverse) stacks moves)
       (map first)
       (apply str)))

(defn part-2 [[stacks moves]]
  (->> (reduce (partial apply-move identity) stacks moves)
       (map first)
       (apply str)))

(defn -main [] (aoc-solve part-1 part-2 (input-parsed "2022/day5.txt" parse-begin-state)))

; part 1: MQTPGLLDN
; part 2: LVZPSTTCZ