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

(defn find-halt
  [instructions]
  (loop [program-counter 0
         accumulator 0
         executed #{}]
    (cond
      (contains? executed program-counter)
        {:accumulator accumulator :halted-at program-counter :executed executed}
      (>= program-counter (count instructions))
        {:accumulator accumulator :halted-at nil :executed executed}
      :else
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

(defn part1
  [input]
  (let [halt-dict (find-halt input)]
    (:accumulator halt-dict)))

(defn terminated-program-result
  [edited-program]
  (let [halt-results (find-halt edited-program)]
    (if (nil? (:halted-at halt-results))
      (:accumulator halt-results)
      nil)))

;; kinda gnarly, just brute force every possible nop/jmp change in the program
(defn part2
  [instructions]
  (loop [candidate-idx 0]
    (let [[op arg] (get-operation-and-argument instructions candidate-idx)]
      (case op
        "nop" (let [edited-instructions (assoc instructions candidate-idx (str "jmp " arg))
                    edited-results (terminated-program-result edited-instructions)]
                (if (nil? edited-results)
                  (recur (inc candidate-idx))
                  edited-results))
        "acc" (recur (inc candidate-idx))
        "jmp" (let [edited-instructions (assoc instructions candidate-idx (str "nop " arg))
                    edited-results (terminated-program-result edited-instructions)]
                (if (nil? edited-results)
                  (recur (inc candidate-idx))
                  edited-results))))))

(def input (->> (slurp "test/advent/day8/input")
                (str/split-lines)))

(comment
  (part1 input)
  (part2 input)
  )
