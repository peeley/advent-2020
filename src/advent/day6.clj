(ns advent.day6
  (:require [clojure.string :as str]))

(defn get-answers-from-person
  [question-answers answer-set]
  (into answer-set question-answers))

(defn get-answers-from-group
  [group]
  (let [individual-answers (str/split-lines group)]
    (loop [answers individual-answers
           yeses #{}]
      (if (empty? answers)
        yeses
        (recur (rest answers) (into yeses (first answers)))))))

(defn num-yeses-in-group
  [group]
  (let [yeses (get-answers-from-group group)]
    (count yeses)))

(defn part1
  [input]
  (let [groups (str/split input #"\n\n")]
    (apply + (map num-yeses-in-group groups))))

(comment
  (def input (slurp "test/advent/day6/input"))
  (get-answers-from-person "abc" #{})
  (num-yeses-in-group "ab\nac")
  (part1 input)
  )
