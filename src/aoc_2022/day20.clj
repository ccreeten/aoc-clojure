(ns aoc-2022.day20
  (:require
    [util.input :refer :all]
    [util.solution :refer :all]))

(defrecord node [id value])
(defrecord link [id left right])
(defrecord linked-list [nodes links size])

(defn get-left [linked-list idx]
  (get-in linked-list [:links idx :left]))

(defn get-right [linked-list idx]
  (get-in linked-list [:links idx :right]))

(defn value [linked-list idx]
  (get-in linked-list [:nodes idx :value]))

(defn update-links [linked-list & updates]
  (reduce (fn [result [idx link value]] (assoc-in result [:links idx link] value)) linked-list updates))

(defn index-of [linked-list val]
  (first (filter #(= (get-in linked-list [:nodes % :value]) val) (range 0 (:size linked-list)))))

(defn get-value [linked-list from idx]
  (loop [from from idx idx]
    (if (zero? idx)
      (get-in linked-list [:nodes from :value])
      (recur (get-right linked-list from) (dec idx)))))

(defn to-linked-list [numbers]
  (let [size (count numbers)
        nodes (mapv #(->node % (get numbers %)) (range 0 size))
        links (mapv #(->link % (mod (dec %) size) (mod (inc %) size)) (range 0 size))]
    (->linked-list nodes links (count nodes))))

(defn move [linked-list idx]
  (let [delta (mod (value linked-list idx) (dec (:size linked-list)))]
    (if (pos? delta)
      (loop [delta delta linked-list linked-list]
        (if (zero? delta)
          linked-list
          (let [x (get-left linked-list idx) y (get-right linked-list idx) z (get-right linked-list y)]
            (recur (dec delta) (update-links linked-list
                                             [x   :right y]
                                             [x   :right y]
                                             [y   :left  x]
                                             [y   :right idx]
                                             [idx :left  y]
                                             [idx :right z]
                                             [z   :left  idx])))))
      (loop [delta delta linked-list linked-list]
        (if (zero? delta)
          linked-list
          (let [x (get-left linked-list idx) y (get-right linked-list idx) z (get-left linked-list x)]
            (recur (inc delta) (update-links linked-list
                                             [y   :left  x]
                                             [x   :right y]
                                             [x   :left  idx]
                                             [idx :right x]
                                             [idx :left  z]
                                             [z   :right idx]))))))))

(defn mix [linked-list]
  (loop [idx 0 linked-list linked-list]
    (if (= idx (:size linked-list))
      linked-list
      (recur (inc idx) (move linked-list idx)))))

(defn decrypt [linked-list key]
  (update linked-list :nodes (fn [nodes] (mapv #(update % :value (partial * key)) nodes))))

(defn coordinate-sum [linked-list cycles]
  (let [mixed (nth (iterate mix linked-list) cycles) start (index-of mixed 0)]
    (->> (list 1000 2000 3000)
         (map #(get-value mixed start %))
         (reduce +))))

(defn part-1 [input]
  (coordinate-sum input 1))

(defn part-2 [input]
  (coordinate-sum (decrypt input 811589153) 10))

(defn -main [] (aoc-solve part-1 part-2 (to-linked-list (vec (input-ints "2022/day20.txt")))))

; part 1 : 16533
; part 2 : 4789999181006