(ns advent.day2
  (:require [clojure.string :as string]))

(defn parse-rule
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

(defn matches-rule?
  "check if a given string matches a password rule"
  [rule]
  (let [{password :password
         letter :letter
         min :min
         max :max} rule
        matches (count (filter #(= % letter) password))]
    (and (>= matches min) (<= matches max))))

(defn part1
  "Count how many passwords match their policy"
  [input]
  (let [lines (string/split-lines input)
        rule (parse-rule (first lines))]
    (count (filter #(matches-rule? (parse-rule %)) lines))))

