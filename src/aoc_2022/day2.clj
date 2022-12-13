(ns aoc-2022.day2
  (:require
    [clojure.string :refer [split]]
    [util.input :refer :all]
    [util.solution :refer :all]))

(def decrypt-map
  {"A" :rock, "B" :paper, "C" :scissors
   "X" :rock, "Y" :paper, "Z" :scissors})

(defn parse-round [line]
  (map decrypt-map (split line #" ")))

(def score-use
  {:rock 1, :paper 2, :scissors 3})

(defn score-win [[opp you]]
  ([3 0 6] (mod (- (score-use opp) (score-use you)), 3)))

(defn score-1 [[_ you :as round]]
  (+ (score-use you) (score-win round)))

; KISS...
(defn score-2 [round]
  (case round
    "A X" 3
    "A Y" 4
    "A Z" 8
    "B X" 1
    "B Y" 5
    "B Z" 9
    "C X" 2
    "C Y" 6
    "C Z" 7))

(defn part-1 [input]
  (reduce + (map score-1 (map parse-round input))))

(defn part-2 [input]
  (reduce + (map score-2 input)))

(defn -main [] (aoc-solve part-1 part-2 (input-lines "2022/day2.txt")))

; part 1: 14375
; part 2: 10274