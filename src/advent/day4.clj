(ns advent.day4
  (:require [clojure.string :as str]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"})

(defn passport-fields-valid?
  [passport-fields]
  (= (conj passport-fields "cid") required-fields))

(defn get-fields-from-passport
  [passport]
  (into #{} (map #(first (str/split % #":")) (str/split passport #"[ \n]"))))

(defn part1
  [input]
  (count
    (filter passport-fields-valid?
      (map get-fields-from-passport input))))

