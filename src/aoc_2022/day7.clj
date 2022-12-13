(ns aoc-2022.day7
  (:require
    [clojure.string :refer [split split-lines starts-with?]]
    [util.collection :refer :all]
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.math :refer :all]
    [util.solution :refer :all]))

(defn command-output? [line]
  (not (starts-with? line "$")))

(defn parse-file [line]
  (parse-int (first (split line #" "))))

(defn parse-ls [output]
  (->> (take-while command-output? output)
       (filter #(not (starts-with? % "dir")))
       (mapv parse-file)))

(defn parse-output [output]
  (loop [[line & rest] output path [0] tree []]
    (cond
      (nil? line) tree
      (re-matches #"\$ cd .." line) (recur rest (update-last (firstv path) inc) tree)
      (re-matches #"\$ cd .*" line) (recur rest path tree)
      (re-matches #"\$ ls"    line) (let [files (parse-ls rest)]
                                    (recur (drop-while command-output? rest) (conj path (count files)) (assoc-in tree path files))))))

(defn size [node]
  (if (vector? node)
    (reduce + (map size node))
    node))

(defn find-directory-sizes [node pred]
  (if (vector? node)
    (flatten (cond-> (map #(find-directory-sizes % pred) node) (pred node) (conj (size node))))
    []))

(defn part-1 [input]
  (sum (find-directory-sizes input #(<= (size %) 100000))))

(defn part-2 [input]
  (let [unused (- 70000000 (size input)) excess (- 30000000 unused)]
    (apply min (find-directory-sizes input #(>= (size %) excess)))))

(defn -main [] (aoc-solve part-1 part-2 (input-parsed "2022/day7.txt" (comp parse-output split-lines))))

; part 1: 2061777
; part 2: 4473403