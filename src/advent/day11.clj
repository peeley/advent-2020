(ns advent.day11
  (:require [clojure.string :as str]))

(def empty-seat \L)
(def occupied-seat \#)
(def floor \.)

(def input (->> (slurp "test/advent/day11/input")
                (str/split-lines)))

(defn get-seat-at
  [[y x] seats]
  (nth (nth seats y nil) x nil))

(defn seat-is-occupied?
  [seat-location seats]
  (= occupied-seat (get-seat-at seat-location seats)))

(defn count-adjacent-occupied
  [[y x] seats]
  (count (filter #(seat-is-occupied? % seats) [[(- y 1) (- x 1)]
                                               [(- y 1) x]
                                               [(- y 1) (+ x 1)]
                                               [y (- x 1)]
                                               [y (+ x 1)]
                                               [(+ y 1) (- x 1)]
                                               [(+ y 1) x]
                                               [(+ y 1) (+ x 1)]])))

(defn no-adjacent-occupied?
  [seat-location seats]
  (= (count-adjacent-occupied seat-location seats) 0))

(defn crowded-seat?
  [seat-location seats]
  (>= (count-adjacent-occupied seat-location seats) 4))

(defn update-seat
  [seat-location seats]
  (let [seat-state (get-seat-at seat-location seats)]
    (cond
      (and (= empty-seat seat-state)
           (no-adjacent-occupied? seat-location seats)) occupied-seat
      (and (= occupied-seat seat-state)
           (crowded-seat? seat-location seats)) empty-seat
      :else seat-state)))

(defn step-state
  [seats-state]
  (let [height (count seats-state)
        width (count (first seats-state))]
    (partition width
               (for [y (range 0 height)
                     x (range 0 width)]
                 (update-seat [y x] seats-state)))))

(defn count-total-occupied
  [seats]
  (->> seats
       (map (fn [row] (filter #(= occupied-seat %) row)))
       (map count)
       (reduce +)))

(defn part1
  [input]
  (loop [before-state input]
    (let [after-state (step-state before-state)]
      (if (= before-state after-state)
        (count-total-occupied after-state)
        (recur after-state)))))
