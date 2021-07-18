(ns advent.day10
  (:require [clojure.string :as str]))

(def input (->> (slurp "test/advent/day10/input")
                (str/split-lines)
                (map #(Integer/parseInt %))
                (sort)
                (vec)
                (#(conj % (+ (reduce max %) 3))))) ;; last is always three higher than max

(defn get-joltage-differences
  [input]
  (loop [differences {1 0 2 0 3 0}
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

(def count-combinations
  (memoize
    (fn [last-joltage current-idx]
      (if (< current-idx (dec (count input)))
        (let [current-joltage (get input current-idx)
              next-joltage (get input (inc current-idx))]
          (if (<= (- next-joltage last-joltage) 3)
            (+ (count-combinations current-joltage (inc current-idx))
               (count-combinations last-joltage (inc current-idx)))
            (count-combinations current-joltage (inc current-idx))))
        1))))

(defn part2
  []
  (count-combinations 0 0))

(part2)
