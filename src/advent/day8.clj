(ns advent.day8
  (:require [clojure.string :as str]))

(defn parse-instruction
  [instruction]
  (let [[op arg-str] (str/split instruction #" ")
        arg (Integer/parseInt arg-str)]
    [op arg]))

(defn get-operation-and-argument
  [instructions index]
  (parse-instruction (nth instructions index)))

(defn part1
  [instructions]
  (loop [program-counter 0
         accumulator 0
         executed #{}]
    (if (contains? executed program-counter)
      accumulator
      (let [[op arg] (get-operation-and-argument instructions program-counter)]
        (case op
          "nop" (recur
                 (inc program-counter)
                 accumulator
                 (conj executed program-counter))
          "acc" (recur
                 (inc program-counter)
                 (+ accumulator arg)
                 (conj executed program-counter))
          "jmp" (recur
                 (+ program-counter arg)
                 accumulator
                 (conj executed program-counter)))))))

