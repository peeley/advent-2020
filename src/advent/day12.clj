(ns advent.day12
  (:require [clojure.string :as str]))

(defn parse-action
  [action-str]
  (let [direction (first action-str)
        magnitude (Integer/parseInt (subs action-str 1))]
    [direction magnitude]))

(defn calc-coordinates
  [x y angle magnitude]
  [(+ x (* magnitude (Math/cos angle)))
   (+ y (* magnitude (Math/sin angle)))
   angle])

(defn perform-action
  [[x y angle] [direction magnitude]]
  (case direction
    \N [x (+ y magnitude) angle]
    \S [x (- y magnitude) angle]
    \E [(+ x magnitude) y angle]
    \W [(- x magnitude) y angle]
    \L [x y (+ angle (Math/toRadians magnitude))]
    \R [x y (- angle (Math/toRadians magnitude))]
    \F (calc-coordinates x y angle magnitude)))

(def input (->> (slurp "test/advent/day12/input")
                (str/split-lines)
                (map parse-action)))
(defn part1
  []
  (let [[x y _] (reduce perform-action [0 0 0] input)]
    (+ (Math/abs x) (Math/abs y))))
