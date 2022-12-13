(ns util.solution)

(defn print-seq-solution [seq]
  (run! (partial println ";") seq))

(defn print-solution [n result]
  (if (seq? result)
    (do (println "; part" n ":") (print-seq-solution result))
    (println "; part" n ":" result)))

(defn aoc-solve [part-1 part-2 input]
  (print-solution 1 (part-1 input))
  (print-solution 2 (part-2 input)))
  ;(let [solution-1 (part-1 input) solution-2 (part-2 input)]
  ;  (print "; part 1:")
  ;  (if (seq? solution-1)
  ;    (print-seq-solution solution-1)
  ;    (println ";" solution-1))
  ;  (print "; part 2:")
  ;  (if (seq? solution-2)
  ;    (print-seq-solution solution-2)
  ;    (println ";" solution-2))))