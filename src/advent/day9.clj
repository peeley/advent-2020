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
  (for [idx (range 25 (count input))
        :let [complement-set (subvec input (- idx 25) idx)
              target (nth input idx)]
        :when (not (subvec-contains-sum? complement-set target))]
    target))

(def input (->> (slurp "test/advent/day9/input")
             (str/split-lines)
             (map #(bigint %))
             (vec)))

(part1 input) 
