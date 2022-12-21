(ns aoc-2022.day16
  (:require
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.math :refer :all]
    [util.solution :refer :all]))

(defrecord valve [name flow neighbours])

(defn parse-valve [line]
  (let [valves (re-seq #"[A-Z]{2}" line) flow (re-seq #"\d+" line)]
    (->valve (first valves) (parse-int (first flow)) (rest valves))))

(defn expand-1 [lookup valve opened time released heuristic]
  (let [tick (dec time) {:keys [flow neighbours]} (get lookup valve)]
    (cond-> (->> neighbours (map #(vector % opened tick released heuristic)))
            (and (> flow 0) (not (opened valve)))
            (conj (vector valve (conj opened valve) tick (+ released (* tick flow)) (- heuristic flow))))))

(defn max-release-1 [valves start time]
  (let [lookup (into {} (map #(vector (:name %) %) valves))]
    (loop [stack (list [start #{} time 0 (sum (map #(:flow %) valves))]) memo #{} max-released 0]
      (if (empty? stack)
        max-released
        (let [[valve opened time released heuristic] (first stack) tail (rest stack)]
          (cond (= time 0) (recur tail memo (max max-released released))
                (memo [valve opened time released]) (recur tail memo (max max-released released))
                (< (+ released (* time heuristic)) max-released) (recur tail memo (max max-released released))
                :else (recur (concat (expand-1 lookup valve opened time released heuristic) tail)
                             (conj memo [valve opened time released])
                             max-released)))))))

(defn expand-2 [lookup valve-1 valve-2 opened time released heuristic]
  (let [tick (dec time) {flow-1 :flow neighbours-1 :neighbours} (get lookup valve-1) {flow-2 :flow neighbours-2 :neighbours} (get lookup valve-2)]
    (cond-> (for [next-valve-1 neighbours-1 next-valve-2 neighbours-2] (vector next-valve-1 next-valve-2 opened tick released heuristic))
            (and (> flow-1 0) (not (opened valve-1)) (> flow-2 0) (not (opened valve-2)) (not= valve-1 valve-2))
            (conj (vector valve-1 valve-2 (conj opened valve-1 valve-2) tick (+ released (* tick flow-1) (* tick flow-2)) (- heuristic flow-1 flow-2)))
            (and (> flow-1 0) (not (opened valve-1)) (not= valve-1 valve-2))
            (concat (->> neighbours-2 (map #(vector valve-1 % (conj opened valve-1) tick (+ released (* tick flow-1)) (- heuristic flow-1)))))
            (and (> flow-2 0) (not (opened valve-2)) (not= valve-1 valve-2))
            (concat (->> neighbours-1 (map #(vector % valve-2 (conj opened valve-2) tick (+ released (* tick flow-2)) (- heuristic flow-2))))))))

(defn max-release-2 [valves start-1 start-2 time]
  (let [l (into {} (map #(vector (:name %) %) valves))
        lookup
        (reduce (fn [m k] (let [res (update-in m [k :neighbours] (fn [coll] (sort-by #(:flow (get m %)) > coll)))] res)) l (keys l))]
    (loop [stack (list [start-1 start-2 #{} time 0 (sum (map #(:flow %) valves))]) memo #{} max-released 0]
      (if (empty? stack)
        max-released
        (let [[valve-1 valve-2 opened time released heuristic] (first stack) tail (rest stack)]
          (cond (= time 0) (recur tail memo (max max-released released))
                (memo [valve-1 valve-2 opened time released]) (recur tail memo (max max-released released))
                (< (+ released (* time heuristic)) max-released) (recur tail memo (max max-released released))
                :else (recur (concat (expand-2 lookup valve-1 valve-2 opened time released heuristic) tail)
                             (conj memo [valve-1 valve-2 opened time released] [valve-2 valve-1 opened time released])
                             max-released)))))))

(defn part-1 [input]
  (max-release-1 input "AA" 1))

; very slow, but gets there eventually (...eventually, after reusing max between separate runs)
; can't be bothered to improve, but I guess the optimization would be to not single step every time,
; but build a distance map using all 0 pressure valves (and since there are so many, the resulting combinations are few)
(defn part-2 [input]
  (max-release-2 input "AA" "AA" 26))

(defn -main [] (aoc-solve part-1 part-2 (map parse-valve (input-lines "2022/day16.txt"))))

; part 1 : 1638
; part 2 : 2400