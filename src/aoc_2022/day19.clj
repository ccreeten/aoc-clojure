(ns aoc-2022.day19
  (:require
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.math :refer :all]
    [util.solution :refer :all]))

(defrecord blueprint [id costs])

(defn parse-blueprint [line]
  (let [numbers (mapv parse-int (re-seq #"\d+" line))]
    (->blueprint (get numbers 0) [[(get numbers 1) 0 0 0]
                                  [(get numbers 2) 0 0 0]
                                  [(get numbers 3) (get numbers 4) 0 0]
                                  [(get numbers 5) 0 (get numbers 6) 0]])))

(defn max-costs [blueprint]
  (assoc (apply mapv max (:costs blueprint)) 3 999))

(def max-costs-memo (memoize max-costs))

(defn predict-geodes [[_ _ _ geo-bots] [_ _ _ geos] time]
  (+ geos (sum-range geo-bots (+ geo-bots time))))

(defn can-pay [collected cost]
  (every? (partial <= 0) (map - collected cost)))

(defn expand [blueprint [bots collected time] stack]
  (let [costs (:costs blueprint) max-costs (max-costs-memo blueprint) collect (mapv + collected bots) tick (dec time)]
    (concat
      (->> (range 0 4)
           (filter #(and (< (bots %) (max-costs %)) (can-pay collected (costs %))))
           (map #(vector (mapv + bots (assoc [0 0 0 0] % 1)) (mapv - collect (costs %)) tick)))
      (conj stack [bots collect tick]))))

(defn max-geodes [blueprint time]
  (loop [stack (list [[1 0 0 0] [0 0 0 0] time]) seen #{} geos 0]
    (if (empty? stack)
      geos
      (let [[bots collected time :as frame] (first stack)]
        (if (or (= time 0)
                (<= (predict-geodes bots collected time) geos)
                (seen frame))
          (recur (rest stack) seen (max geos (last collected)))
          (recur (concat (expand blueprint frame (rest stack))) (conj seen frame) (max geos (last collected))))))))

(defn part-1 [input]
  (reduce + (map #(* (max-geodes % 24) (:id %)) input)))

(defn part-2 [input]
  (reduce * (map #(max-geodes % 32) (take 3 input))))

(defn -main [] (aoc-solve part-1 part-2 (mapv parse-blueprint (input-lines "2022/day19.txt"))))

; part 1 : 1092
; part 2 : 3542