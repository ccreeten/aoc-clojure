(ns aoc-2022.day10
  (:require
    [util.conversion :refer :all]
    [util.solution :refer :all]
    [util.math :refer :all]
    [util.input :refer :all]))

(defrecord addx [cycles value])
(defrecord noop [cycles])

(defn parse-op [line]
  (cond
    (= "noop" line)             (->noop 1)
    (re-matches #"addx.*" line) (->addx 2 (parse-int (first (re-seq #"-?\d+" line))))))

(defn execute [op x]
  (condp = (type op)
    addx (+ x (-> op :value))
    noop (+ x 0)))

(defn signal-strength [ops samples]
  (loop [[op & more] ops x 1 cycle 1 strength 0 samples samples]
    (let [at-sample?        (= (first samples) cycle)
          remaining-samples (cond-> samples at-sample? rest)
          new-strength      (cond-> strength at-sample? (+ (* cycle x)))]
      (cond (empty? remaining-samples) new-strength
            (= (-> op :cycles) 1)      (recur more (execute op x) (inc cycle) new-strength remaining-samples)
            :else                      (recur (conj more (update op :cycles dec)) x (inc cycle) new-strength remaining-samples)))))

(defn render [ops]
  (loop [[op & ops] ops x 1 col 0 output []]
    (let [char (if (<= (abs (- x col)) 1) "#" ".")
          new-col (rem (inc col) 40)]
      (cond (nil? op)             output
            (= (-> op :cycles) 1) (recur ops (execute op x) new-col (conj output char))
            :else                 (recur (conj ops (update op :cycles dec)) x new-col (conj output char))))))

(defn part-1 [input]
  (signal-strength input [20 60 100 140 180 220]))

(defn part-2 [input]
  (map (partial apply str) (partition 40 (render input))))

(defn -main [] (aoc-solve part-1 part-2 (map parse-op (input-lines "2022/day10.txt"))))

; part 1 : 15220
; part 2 :
; ###..####.####.####.#..#.###..####..##..
; #..#.#.......#.#....#.#..#..#.#....#..#.
; #..#.###....#..###..##...###..###..#..#.
; ###..#.....#...#....#.#..#..#.#....####.
; #.#..#....#....#....#.#..#..#.#....#..#.
; #..#.#....####.####.#..#.###..#....#..#.
