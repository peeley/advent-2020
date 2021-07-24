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

(defn rotate-waypoint
  [[waypoint-x waypoint-y] [direction magnitude]]
  (let [angle (Math/atan2 waypoint-y waypoint-x)
        hyp (Math/hypot waypoint-x waypoint-y)
        new-angle (if (= direction \L)
                    (+ angle (Math/toRadians magnitude))
                    (- angle (Math/toRadians magnitude)))
        new-x-dist (* hyp (Math/cos new-angle))
        new-y-dist (* hyp (Math/sin new-angle))]
    [new-x-dist new-y-dist]))

(defn move-towards-waypoint
  [[boat-x boat-y] [waypoint-x waypoint-y] magnitude]
  (let [moved-x (* magnitude waypoint-x)
        moved-y (* magnitude waypoint-y)]
    [(+ boat-x moved-x)
     (+ boat-y moved-y)]))

(defn perform-waypoint-action
  [[boat-coords waypoint-coords] action]
  (let [[waypoint-x waypoint-y] waypoint-coords
        [direction magnitude] action]
   (case direction
     \N [boat-coords [waypoint-x (+ waypoint-y magnitude)]]
     \S [boat-coords [waypoint-x (- waypoint-y magnitude)]]
     \E [boat-coords [(+ waypoint-x magnitude) waypoint-y]]
     \W [boat-coords [(- waypoint-x magnitude) waypoint-y]]
     \L [boat-coords (rotate-waypoint waypoint-coords action)]
     \R [boat-coords (rotate-waypoint waypoint-coords action)]
     \F [(move-towards-waypoint boat-coords waypoint-coords magnitude)
         waypoint-coords])))

(defn part2
  [input]
  (let [[[x y] _] (reduce perform-waypoint-action [[0 0] [10 1]] input)]
    (+ (Math/abs x) (Math/abs y))))
