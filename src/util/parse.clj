(ns util.parse
  (:require
    [util.conversion :refer :all]))

(defn all-ints [line]
  (map parse-int (re-seq #"\d+" line)))

(defn first-int [line]
  (first (all-ints line)))
