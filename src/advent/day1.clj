(ns advent.day1
  (:require [clojure.string]))

(defn part1
  "Find the product of two numbers that sum to 2020"
  [input]
  (loop [complements #{}
         numbers (clojure.string/split-lines input)]
    (let [number (Integer/parseInt (first numbers))
          complement (- 2020 number)]
      (if (contains? complements complement)
        (* number complement)
        (recur (conj complements number) (rest numbers))))))
