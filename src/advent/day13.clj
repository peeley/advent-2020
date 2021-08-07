(ns advent.day13
  (:require [clojure.string :as str]))

(def input
  (as-> (slurp "test/advent/day13/input") lines
    (str/split-lines lines)
    [(Integer/parseInt (first lines))
     (as-> (second lines) bus-ids
       (str/split bus-ids #",")
       (map-indexed
        #(if (= %2 "x") [nil nil] [%1 (Integer/parseInt %2)])
        bus-ids)
       (filter #(not= % [nil nil]) bus-ids))]))

(defn closest-bus-time
  [arrival-time bus-id]
  (*' bus-id
     (Math/ceil (double (/ arrival-time bus-id)))))

(defn get-bus-wait-time
  [arrival-time bus-id]
  (-' (closest-bus-time arrival-time bus-id)
     arrival-time))

(defn compare-wait-times
  [[best-id-so-far best-wait-so-far]
   [this-id this-wait]]
  (if (< this-wait best-wait-so-far)
    [this-id this-wait]
    [best-id-so-far best-wait-so-far]))

(defn part1
  [[arrival-time bus-id-pairs]]
  (let [bus-ids (map second bus-id-pairs)]
    (as-> bus-ids $
      (map #(vec [% (get-bus-wait-time arrival-time %)]) $)
      (reduce compare-wait-times [0 Integer/MAX_VALUE] $)
      (apply * $))))

(defn arrival-time-at-offset?
  [time [bus-id-idx bus-id]]
  (let [closest-time (closest-bus-time time bus-id)
        difference (-' closest-time time)]
    (== difference bus-id-idx)))

(defn arrival-times-increase?
  [time bus-id-pairs]
  (as-> bus-id-pairs $
    (map #(arrival-time-at-offset? time %) $)
    (every? identity $)))

(defn part2
  [[_ bus-ids]]
  ;; according to the problem, this is likely the earliest time since there's so
  ;; many bus ids
  (loop [time 0N]
    (if (arrival-times-increase? time bus-ids)
      time
      (recur (inc time)))))

(comment
  input

  (part2 input)

  (part2 [nil [[0 1789] [1 37] [2 47] [3 1889]]])
  (map #(closest-bus-time 51 %) [17 13 19])
  (*' 2 2)
  (apply < [1 3 2])
  )
