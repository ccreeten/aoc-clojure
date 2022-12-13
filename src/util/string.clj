(ns util.string
  (:require
    [clojure.string :refer [blank?]]))

(defn not-blank? [s]
  (not (blank? s)))
