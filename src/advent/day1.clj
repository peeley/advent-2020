(ns advent.day1
  (:require [clojure.string]))

(defn part1
  "Find the product of two numbers that sum to 2020"
  [input]
  (loop [complements #{}
         numbers (clojure.string/split-lines input)]
    (let [number (Integer/parseInt (first numbers))]
      (if (contains? complements (- 2020 number))
        (* number (- 2020 number))
        (recur (conj complements number) (rest numbers))))))

