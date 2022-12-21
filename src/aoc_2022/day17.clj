(ns aoc-2022.day17
  (:require
    [util.input :refer :all]
    [util.solution :refer :all]))

(def pieces [[[0 0], [1 0], [2 0], [3 0]]
             [[1 0], [0 1], [1 1], [2 1], [1 2]]
             [[0 0], [1 0], [2 0], [2 1], [2 2]]
             [[0 0], [0 1], [0 2], [0 3]]
             [[0 0], [1 0], [0 1], [1 1]]])

(def deltas {\< -1, \> +1})

(defn free? [x-offset y-offset piece filled]
  (and (>= x-offset 0)
       (>= y-offset 0)
       (every? #(and (< (+ (first %) x-offset) 7) (not (filled (map + % [x-offset y-offset])))) piece)))

(defn fill [[part & more] x-offset y-offset filled]
  (if (nil? part)
    filled
    (fill more x-offset y-offset (conj filled (map + part [x-offset y-offset])))))

(defn drop-piece [piece jets filled height]
  (loop [[jet & stream] jets x-offset 2 y-offset (+ height 4)]
    (let [delta-x (get deltas jet)
          moved-x (cond-> x-offset (free? (+ x-offset delta-x) y-offset piece filled) (+ delta-x))
          moved-y (cond-> y-offset (free? moved-x (+ y-offset -1) piece filled) (- 1))]
      (if (= y-offset moved-y)
        [stream (fill piece moved-x moved-y filled) (max height (+ moved-y (apply max (map second piece))))]
        (recur stream moved-x moved-y)))))

(defn simulate [jet pieces cycles]
  (loop [pieces (cycle pieces) jets (cycle jet) filled #{} height -1 loop-detect [] cycle cycles]
    (let [loop-detect (cond-> loop-detect
                              (every? filled (map #(vector % height) (range 0 7)))
                              (conj [height cycle]))]
      (cond (= cycle 0) (inc height)
            (< (count loop-detect) 2) (let [[stream filled height] (drop-piece (first pieces) jets filled height)]
                                        (recur (rest pieces) stream filled height loop-detect (dec cycle)))
            :else (let [[[height-1 cycle-1] [height-2 cycle-2]] loop-detect
                        cycle-delta (- cycle-1 cycle-2)
                        height-delta (- height-2 height-1)
                        repeat-cycles (quot cycle cycle-delta)]
                    (+ 1 height (* repeat-cycles height-delta) (simulate jets pieces (- cycle (* repeat-cycles cycle-delta)))))))))

(defn part-1 [input]
  (simulate input pieces 2022))

(defn part-2 [input]
  (simulate input pieces 1000000000000))

(defn -main [] (aoc-solve part-1 part-2 (input-all "2022/day17.txt")))

; part 1 : 3157
; part 2 : 1581449275319