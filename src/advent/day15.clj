(ns advent.day15
  (:require [clojure.string :as str]))

(def input (as-> "8,13,1,0,18,9" $
                (str/split $ #",")
                (map #(Integer/parseInt %) $)))

(defn part1
  [input]
  (loop [ages (into {} (map-indexed #(into [] [(str %2) (inc %1)]) input))
         turn (inc (count input))
         last-number (last input)]
    (if (= turn 2021)
      last-number
      (let [last-turn-spoken (ages (str last-number))
            new-number (if (nil? last-turn-spoken)
                         0
                         (- turn last-turn-spoken))
            new-ages (assoc ages (str new-number) turn)]
        (recur new-ages (inc turn) new-number)))))

(comment
  (part1 input)

  (def ages (into {} (map-indexed #(into [] [(str %2) (inc %1)]) input)))
  (def last-number (last input))
  (def turn (inc (count input)))
  (ages (str last-number))
  (def new-number (- (inc (count input)) (ages (str last-number))))
  (assoc ages (str new-number) turn)
  (ages "9")
  )
