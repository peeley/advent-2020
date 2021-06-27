(ns advent.day5
  (:require [clojure.string :as str]))

(defn calc-midpoint
  [min max]
  (int
   (+
    (/ (- max min)
       2)
    min)))

(defn process-partition-character
  [[min max] character]
  (let [midpoint (calc-midpoint min max)]
    (case character
      \F [min midpoint]
      \L [min midpoint]
      \B [midpoint max]
      \R [midpoint max])))

(defn process-partition-string
  [[min max] partition-string]
  (loop [characters partition-string
         row-interval [min max]]
    (if (empty? characters)
      (last row-interval)
      (recur
       (str/join (rest characters))
       (process-partition-character row-interval (first characters))))))

(defn get-row-number
  [row-partitions]
  (process-partition-string [0 127] row-partitions))

(defn get-col-number
  [col-partitions]
  (process-partition-string [0 7] col-partitions))

(defn get-seat-number
  [partition-string]
  (let [row-partitions (take 7 partition-string)
        col-partitions (take-last 3 partition-string)]
    (+ (get-col-number col-partitions)
       (* 8
          (get-row-number row-partitions)))))

(defn convert-input-to-seat-numbers
  [input-string]
  (map
    get-seat-number
    (str/split-lines input-string)))

(defn part1
  [input-string]
  (apply max
   (convert-input-to-seat-numbers input-string)))

(defn seat-is-after-gap?
  [last-seat this-seat]
  (< 1 (- this-seat last-seat)))

(defn part2
  [input-string]
  (let [seat-numbers (convert-input-to-seat-numbers input-string)
        sorted-seat-numbers (sort seat-numbers)]
    (loop [seats sorted-seat-numbers
           last-seat-checked Integer/MAX_VALUE]
      (let [this-seat (first seats)]
        (if (seat-is-after-gap? last-seat-checked this-seat)
          (- this-seat 1)
          (recur (rest seats) this-seat))))))
