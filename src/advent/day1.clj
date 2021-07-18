(ns advent.day1
  (:require [clojure.string]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

(def input (->> (slurp "test/advent/day1/input")
                (str/split-lines)
                (map #(Integer/parseInt %))
                (vec)))

(defn part1
  "Find the product of two numbers that sum to 2020"
  [input]
  (loop [complements #{}
         numbers input]
    (let [number (Integer/parseInt (first input))
          complement (- 2020 number)]
      (if (contains? complements complement)
        (* number complement)
        (recur (conj complements number) (rest numbers))))))

(defn part2
  [input target]
  (let [eligible-nums (filter #(< % target) input)
        combos (combo/combinations eligible-nums 3)
        sum-to-target (first (filter #(= target (apply + %)) combos))]
    (apply * sum-to-target)))

