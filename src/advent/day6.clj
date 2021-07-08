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

(defn num-people-in-group
  [group]
  (count (str/split-lines group)))

(defn insert-answer-into-occurence-map
  [answer-map answer-char]
  (let [occurence (get answer-map answer-char 0)]
    (conj answer-map {answer-char (+ 1 occurence)})))

(defn insert-answer-line-into-occurence-map
  [answer-map answer-line]
  (reduce insert-answer-into-occurence-map answer-map answer-line))

(defn get-answer-occurences-from-group
  [group]
  (let [individual-answers (str/split-lines group)]
    (reduce insert-answer-line-into-occurence-map {} individual-answers)))

(defn get-number-of-unanimous-answers-from-group
  [group]
  (let [num-people (num-people-in-group group)
        answer-occurences (get-answer-occurences-from-group group)]
    (count (filter #(= (second %) num-people) answer-occurences))))

(defn part2
  [input]
  (let [groups (str/split input #"\n\n")]
    (apply + (map get-number-of-unanimous-answers-from-group groups))))
