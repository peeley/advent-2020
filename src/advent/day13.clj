(ns advent.day13
  (:require [clojure.string :as str]))

(def input
  (as-> (slurp "test/advent/day13/input") $
    (str/split-lines $)
    [(Integer/parseInt (first $))
     (map #(Integer/parseInt %) (filter #(not= % "x") (str/split (second $) #",")))]))

(defn closest-bus-time
  [arrival-time bus-id]
  (* bus-id
     (Math/ceil (/ arrival-time bus-id))))

(defn get-bus-wait-time
  [arrival-time bus-id]
  (- (closest-bus-time arrival-time bus-id)
     arrival-time))

(defn compare-wait-times
  [[best-id-so-far best-wait-so-far]
   [this-id this-wait]]
  (if (< this-wait best-wait-so-far)
    [this-id this-wait]
    [best-id-so-far best-wait-so-far]))

(defn part1
  [[arrival-time bus-ids]]
  (as-> bus-ids $
    (map #(vec [% (get-bus-wait-time arrival-time %)]) $)
    (reduce compare-wait-times [0 Integer/MAX_VALUE] $)
