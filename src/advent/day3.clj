(ns advent.day3
  (:require [clojure.string :as string]))

(defn get-tile
  [lines x y]
  (let [width (count (first lines))]
    (nth (nth lines y) (mod x width))))

(defn part1
  [lines]
  (let [height (count lines)]
    (loop [x 0
           y 0
           count 0]
      (if (>= y height)
        count
        (recur
         (+ x 3)
         (+ y 1)
         (if (= (get-tile lines x y) \#) (inc count) count))))))
