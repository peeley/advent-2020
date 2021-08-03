(ns advent.day15
  (:require [clojure.string :as str]))

(def input (as-> "8,13,1,0,18,9" $
                (str/split $ #",")
                (map #(Integer/parseInt %) $)))

(defn part1
  [input]
  (loop [new-numbers {}
         previous-numbers (into {} (map-indexed #(vector %2 %1) input))
         turn (count input)
         last-number (last input)]
    (if (= turn 2020)
      last-number
      (let [new-number (if (and (new-numbers last-number)
                                (previous-numbers last-number))
                         (- (previous-numbers last-number)
                            (new-numbers last-number))
                         0)]
        (recur (assoc new-numbers new-number (previous-numbers new-number))
               (assoc previous-numbers new-number turn)
               (inc turn)
               new-number)))))
