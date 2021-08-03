(ns advent.day15
  (:require [clojure.string :as str]))

(def input (as-> "8,13,1,0,18,9" $
                (str/split $ #",")
                (map #(Integer/parseInt %) $)))

(defn find-nth-number-spoken
  [starting-nums n]
  (loop [new-numbers {}
         previous-numbers (into {} (map-indexed #(vector %2 %1) starting-nums))
         turn (count starting-nums)
         last-number (last starting-nums)]
    (if (= turn n)
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

(defn part1
  [input]
  (find-nth-number-spoken input 2020))

(defn part2
  [input]
  (find-nth-number-spoken input 30000000))
