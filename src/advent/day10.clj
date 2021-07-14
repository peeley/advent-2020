(ns advent.day10
  (:require [clojure.string :as str]))

(def input (->> (slurp "test/advent/day10/input")
                (str/split-lines)
                (map #(Integer/parseInt %))
                (vec)
                (sort)))

(defn get-joltage-differences
  [input]
  (loop [differences {1 0 2 0 3 1}
         last-joltage 0
         joltage-ratings input]
    (if (empty? joltage-ratings)
      differences
      (let [this-joltage (first joltage-ratings)
            diff (- this-joltage last-joltage)]
        (recur (update differences diff inc)
               this-joltage
               (rest joltage-ratings))))))

(defn part1
  [input]
  (let [differences (get-joltage-differences input)
        one-jolt-diffs (get differences 1)
        three-jolt-diffs (get differences 3)]
    (* one-jolt-diffs three-jolt-diffs)))

(part1 input)
