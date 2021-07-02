(ns advent.day7
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn separate-bag-color-from-number
  [str]
  (second (str/split str #"[0-9] ")))

(defn parse-input-line
  [line]
  (let [[outer-bag inner-bags-str] (str/split line #" bags contain ")
        inner-bags-list (map separate-bag-color-from-number (str/split inner-bags-str #" bag[s]?[,.]"))]
    {outer-bag (set inner-bags-list)}))

(defn parse-input-to-graph
  [input]
  (let [input-lines (str/split-lines input)]
    (apply merge (map #(parse-input-line %) input-lines))))

(def input (slurp "test/advent/day7/input"))

(def graph (parse-input-to-graph input))

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
