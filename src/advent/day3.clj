(ns advent.day3
  (:require [clojure.string :as string]))

(defn get-tile
  [lines x y]
  (let [width (count (first lines))]
    (nth (nth lines y) (mod x width))))

(defn part1
  [lines x-slope y-slope]
  (let [height (count lines)]
    (loop [x 0
           y 0
           count 0]
      (if (>= y height)
        count
        (recur
         (+ x x-slope)
         (+ y y-slope)
         (if (= (get-tile lines x y) \#) (inc count) count))))))


(defn part2
  [input]
  (reduce *
          (map #(advent.day3/part1 input (first %) (second %))
               [[1 1] [3 1] [5 1] [7 1] [1 2]])))
