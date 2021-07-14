(ns advent.day9
  (:require [clojure.string :as str]))

(defn complement-in-set?
  [target set x]
  (contains? (disj set x) (- target x)))

(defn subvec-contains-sum?
  [subvec sum]
  (let [subset (set subvec)]
    (some identity (map #(complement-in-set? sum subset %) subvec))))

(defn part1
  [input]
  (first 
    (for [idx (range 25 (count input))
            :let [complement-set (subvec input (- idx 25) idx)
                target (nth input idx)]
            :when (not (subvec-contains-sum? complement-set target))]
        target)))

(def input (->> (slurp "test/advent/day9/input")
             (str/split-lines)
             (map #(bigint %))
             (vec)))

(def invalid-number (part1 input))

(defn get-interval-indices
  [input]
  (loop [window-min 0
         window-max 1]
    (let [sum (apply + (subvec input window-min window-max))]
      (cond
        (= sum invalid-number) [window-min window-max]
        (< sum invalid-number) (recur window-min (inc window-max))
        (> sum invalid-number) (recur (inc window-min) window-max)))))

(defn part2
  [input]
  (let [[start end] (get-interval-indices input)
        subset (subvec input start end)
        min (apply min subset)
        max (apply max subset)]
    (+ min max)))

(part2 input)
