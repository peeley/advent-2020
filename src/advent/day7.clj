(ns advent.day7
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn separate-bag-color-from-number
  [str]
  (second (str/split str #"[0-9] ")))

(defn separate-number-from-bag-color
  [str]
  (Integer/parseInt (re-find #"\d+" str)))

(defn extract-color-and-number
  [str]
  (if (.contains str "no other")
    []
    (let [color (separate-bag-color-from-number str)
          number (separate-number-from-bag-color str)]
      [number color])))

(defn parse-input-line
  [line]
  (let [[outer-bag inner-bags-str] (str/split line #" bags contain ")
        inner-bags-list (map separate-bag-color-from-number (str/split inner-bags-str #" bag[s]?[,.]"))]
    {outer-bag (set inner-bags-list)}))

(defn parse-input-line-with-number
  [line]
  (let [[outer-bag inner-bags-str] (str/split line #" bags contain ")
        inner-bags-list (map extract-color-and-number (str/split inner-bags-str #" bag[s]?[,.]"))]
    {outer-bag inner-bags-list}))

(defn parse-input-to-graph
  [input]
  (let [input-lines (str/split-lines input)]
    (apply merge (map #(parse-input-line %) input-lines))))

(defn parse-input-to-weighted-graph
  [input]
  (let [input-lines (str/split-lines input)]
    (apply merge (map #(parse-input-line-with-number %) input-lines))))

(def input (slurp "test/advent/day7/input"))

(def graph (parse-input-to-graph input))

(def weighted-graph (parse-input-to-weighted-graph input))

(defn get-color-sub-bags
  [color]
  (get graph color))

(defn bag-is-empty?
  [outer-bag]
  (empty? (get-color-sub-bags outer-bag)))

(defn bag-holds-color?
  [outer-bag color]
  (contains? (get-color-sub-bags outer-bag) color))

(defn get-bags-containing-color
  [current-bag target-color previous-bags]
    (cond
      (contains? previous-bags current-bag) #{}
      (bag-is-empty? current-bag) #{}
      (bag-holds-color? current-bag target-color) (conj previous-bags current-bag)
      :else
      (let [sub-bags (get-color-sub-bags current-bag)
            bags-so-far (conj previous-bags current-bag)]
          (apply set/union (map #(get-bags-containing-color % target-color bags-so-far) sub-bags)))))

(defn part1
  [input]
  (let [graph (parse-input-to-graph input)]
    (count
     (apply set/union
       (map #(get-bags-containing-color % "shiny gold" #{}) (keys graph))))))

(defn count-number-of-bags
  [bag-tuple]
  (if (= bag-tuple [])
    0
    (let [[number color] bag-tuple]
      (+ number (* number (get-total-number-of-bags-inside color))))))

(defn get-total-number-of-bags-inside
  [outer-color]
  (if (bag-is-empty? outer-color)
    1
    (let [inner-bags (get weighted-graph outer-color)]
      (apply +
        (map
          #(count-number-of-bags %)
          inner-bags)))))
