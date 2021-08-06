(ns advent.day11
  (:require [clojure.string :as str]))

(def empty-seat \L)
(def occupied-seat \#)
(def floor \.)

(def input (->> (slurp "test/advent/day11/input")
                (str/split-lines)))

(defn get-seat-at
  [[y x] seats]
  (get (get seats y) x))

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
  [seat-location seats crowded-pred empty-pred]
  (let [seat-state (get-seat-at seat-location seats)]
    (cond
      (and (= empty-seat seat-state)
           (empty-pred seat-location seats)) occupied-seat
      (and (= occupied-seat seat-state)
           (crowded-pred seat-location seats)) empty-seat
      :else seat-state)))

(defn step-state
  [seats-state crowded-pred empty-pred]
  (let [height (count seats-state)
        width (count (first seats-state))]
    (mapv vec
          (partition width
                     (for [y (range 0 height)
                           x (range 0 width)]
                       (update-seat [y x]
                                    seats-state
                                    crowded-pred
                                    empty-pred))))))

(defn count-total-occupied
  [seats]
  (->> seats
       (map (fn [row] (filter #(= occupied-seat %) row)))
       (map count)
       (reduce +)))

(defn part1
  [input]
  (loop [before-state input]
    (let [after-state (step-state before-state
                                  crowded-seat?
                                  no-adjacent-occupied?)]
      (if (= before-state after-state)
        (count-total-occupied after-state)
        (recur after-state)))))

(defn out-of-bounds?
  [seats-state coords]
  (let [[y x] coords
        height (count seats-state)
        width (count (first seats-state))]
    (or (or (< x 0)
            (> x width))
        (or (< y 0)
            (> y height)))))

(defn update-coords-in-direction
  [coords direction]
  (let [[y x] coords]
    (case direction
      :nw [(- y 1) (- x 1)]
      :n  [(- y 1) x]
      :ne [(- y 1) (+ x 1)]
      :w  [y (- x 1)]
      :e  [y (+ x 1)]
      :sw [(+ y 1) (- x 1)]
      :s  [(+ y 1) x]
      :se [(+ y 1) (+ x 1)])))

(defn occupied-seat-in-direction?
  [seats-state coords direction]
  (loop [coords (update-coords-in-direction coords direction)]
    (if (out-of-bounds? seats-state coords)
      false
      (let [seat-type (get-seat-at coords seats-state)]
        (case seat-type
          \# true
          \L false
          \. (recur (update-coords-in-direction coords direction))
          false)))))

(defn num-occuped-seats-in-each-direction
  [seats-state seat-coords]
  (let [directions [:nw :n :ne
                    :w     :e
                    :sw :s :se]]
    (->> directions
         (map #(occupied-seat-in-direction? seats-state seat-coords %))
         (filter identity)
         (count))))

(defn crowded-seats-in-sight?
  [seat-coords seats-state]
  (< 4 (num-occuped-seats-in-each-direction seats-state seat-coords)))

(defn no-occupied-in-sight?
  [seat-coords seats-state]
  (= 0 (num-occuped-seats-in-each-direction seats-state seat-coords)))

(defn part2
  [input]
  (loop [before-state input]
    (let [after-state (step-state before-state
                                  crowded-seats-in-sight?
                                  no-occupied-in-sight?)]
      (if (= before-state after-state)
        (count-total-occupied after-state)
        (recur after-state)))))
