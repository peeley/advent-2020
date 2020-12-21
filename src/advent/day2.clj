(ns advent.day2
  (:require [clojure.string :as string]))

(defn parse-line
  "Parse line of text into rule and password"
  [rule]
  (let [halves (string/split rule #":")
        password (second halves)
        rule-parts (string/split (first halves) #" ")
        letter (first (second rule-parts))
        min-max (string/split (first rule-parts) #"-")
        min (Integer/parseInt (first min-max))
        max (Integer/parseInt (second min-max))]
    {:password password
     :letter letter
     :min min
     :max max}))

(defn counts-valid?
  "check if a given string has required amount of letter"
  [line]
  (let [{password :password
         letter :letter
         min :min
         max :max} line
        matches (count (filter #(= % letter) password))]
    (and (>= matches min) (<= matches max))))

(defn matches-rule?
  [input policy]
  (let [lines (string/split-lines input)]
    (count (filter #(policy (parse-line %)) lines))))

(defn part1
  "Count how many passwords match their policy"
  [input]
  (matches-rule? input counts-valid?))

(defn xor
  [a b]
  (or (and a (not b)) (and (not a) b)))

(defn positions-valid?
  "check if letters in proper position according to rule"
  [rule]
  (let [{password :password
         letter :letter
         min :min
         max :max} rule]
    (xor
     (= (nth password min) letter)
     (= (nth password max) letter))))

(defn part2
  [input]
  (matches-rule? input positions-valid?))
