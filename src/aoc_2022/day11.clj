(ns aoc-2022.day11
  (:require
    [util.conversion :refer :all]
    [util.input :refer :all]
    [util.parse :refer :all]
    [util.solution :refer :all]))

(defrecord monkey [items op test t f inspects])

(defn parse-op [line]
  (let [parts (into [] (re-seq #"old|\+|\*|\d+" line))
        op    (if (= (get parts 1) "*") * +)
        right (if (= (get parts 2) "old") identity (constantly (parse-int (get parts 2))))]
    #(op % (right %))))

(defn parse-monkey [lines]
  (let [items (mapv parse-int (re-seq #"\d+" (get lines 1)))
        op    (parse-op (get lines 2))
        test  (first-int (get lines 3))
        t     (first-int (get lines 4))
        f     (first-int (get lines 5))]
    (->monkey items op test t f 0)))

(defn simulate-turn [monkeys idx manage-worry]
  (let [{:keys [items op test t f]} (get monkeys idx)]
    (loop [[item & more] items monkeys monkeys]
      (if (nil? item)
        (assoc-in monkeys [idx :items] [])
        (let [worry (manage-worry (op item))
              new-monkeys (if (= (rem worry test) 0)
                            (update-in monkeys [t :items] #(conj % worry))
                            (update-in monkeys [f :items] #(conj % worry)))]
          (recur more (update-in new-monkeys [idx :inspects] inc)))))))

(defn simulate-round [monkeys manage-worry]
  (loop [monkeys monkeys idx 0]
    (if (= idx (count monkeys))
      monkeys
      (recur (simulate-turn monkeys idx manage-worry) (inc idx)))))

(defn simulate [monkeys rounds manage-worry]
  (if (= rounds 0)
    monkeys
    (recur (simulate-round monkeys manage-worry) (dec rounds) manage-worry)))

(defn monkey-business [monkeys rounds manage-worry]
  (->> (simulate monkeys rounds manage-worry)
       (map #(-> % :inspects))
       (sort >)
       (take 2)
       (apply *)))

(defn part-1 [input] (monkey-business input 20 #(quot % 3)))

(defn part-2 [input]
  (let [comp-divs (reduce * (map #(-> % :test) input))]
    (monkey-business input 10000 #(rem % comp-divs))))

(defn -main [] (aoc-solve part-1 part-2 (mapv parse-monkey (mapv vec (partition-all 7 (input-lines "2022/day11.txt"))))))

; part 1 : 99852
; part 2 : 25935263541