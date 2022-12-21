(ns aoc-2022.day21
  (:require
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.solution :refer :all]))

(defn parse-monkey [line]
  (let [parts (into [] (re-seq #"[a-z]+|\d+|[+\-*/]" line))]
    (if (= 2 (count parts))
      [(parts 0) (parse-int (parts 1))]
      [(parts 0) [(parts 2) (parts 1) (parts 3)]])))

(defn evaluate [monkeys name]
  (let [job (monkeys name)]
    (if (number? job) job (apply (load-string (job 0)) (map #(evaluate monkeys %) (rest job))))))

(defn depends-on-humn? [monkeys name]
  (or (= name "humn")
      (let [job (monkeys name)]
        (and (vector? job) (some #(depends-on-humn? monkeys %) (rest job))))))

(defn solve [monkeys name total]
  (if (= name "humn")
    total
    (let [monkey      (get monkeys name)
          solve-left? (depends-on-humn? monkeys (monkey 1))
          unknown     (if solve-left? (monkey 1) (monkey 2))
          known       (evaluate monkeys (if solve-left? (monkey 2) (monkey 1)))
          op          (monkey 0)]
      (solve monkeys unknown
             (cond (= op "=") known
                   (= op "+") (- total known)
                   (= op "*") (/ total known)
                   (= op "-") (if solve-left? (+ total known) (- known total))
                   (= op "/") (if solve-left? (* total known) (/ known total)))))))

(defn part-1 [input]
  (evaluate input "root"))

(defn part-2 [input]
  (solve (assoc-in input ["root" 0] "=") "root" nil))

(defn -main [] (aoc-solve part-1 part-2 (into {} (map parse-monkey (input-lines "2022/day21.txt")))))

; part 1 : 87457751482938
; part 2 : 3221245824363