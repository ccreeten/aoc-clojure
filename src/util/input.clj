(ns util.input
  (:require
    [clojure.string :refer [split split-lines]]
    [util.conversion :refer :all]))

(defn input-all [file]
  (slurp (clojure.java.io/resource file)))

(defn input-parsed [file f]
  (f (slurp (clojure.java.io/resource file))))

(defn input-lines [file]
  (input-parsed file split-lines))

(defn input-split-by [file re]
  (input-parsed file #(split % re)))

(defn input-int-grid [file]
  (mapv #(mapv parse-int (re-seq #"\d" %)) (input-lines file)))