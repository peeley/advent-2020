(ns advent.day14
  (:require [clojure.string :as str]))

(defn parse-mask-to-map
  [mask-str]
  (into {} (for [idx (range 0 (count mask-str))
                 :let [bit (nth (reverse mask-str) idx)]]
             [idx bit])))

(defn modify-bit
  [index bit-state number]
  (case bit-state
    \0 (bit-clear number index)
    \1 (bit-set number index)
    \X number))

(defn mask-value
  [mask value]
  (reduce #(modify-bit (first %2) (second %2) %1) value mask))

(defn modify-memory
  [index val mask mem-cells]
  (let [masked-val (mask-value mask val)]
    (assoc mem-cells index masked-val)))

(defn parse-mask-command
  [string]
  {:type :mask-change
   :mask (parse-mask-to-map (second (str/split string #"= ")))})

(defn parse-mem-insert-command
  [string]
  (let [index (subs string 4 (str/last-index-of string \]))
        val (subs string (inc (str/last-index-of string \space)))]
    {:type :mem-insert
     :index (Integer/parseInt index)
     :val (Integer/parseInt val)}))

(defn parse-command
  [command-str]
  (if (str/includes? command-str "mask")
    (parse-mask-command command-str)
    (parse-mem-insert-command command-str)))

(def input (->> (slurp "test/advent/day14/input")
                (str/split-lines)
                (map parse-command)))

(defn process-commands
  [commands modify-fn]
  (loop [commands commands
         mem-cells {}
         mask 0]
    (if (empty? commands)
      mem-cells
      (let [this-command (first commands)
            rest-commands (rest commands)]
        (case (:type this-command)
          :mem-insert (recur rest-commands
                             (modify-fn
                              (:index this-command)
                              (:val this-command)
                              mask
                              mem-cells)
                             mask)
          :mask-change (recur rest-commands mem-cells (:mask this-command)))))))

(defn part1
  [input]
  (let [mem-cells (process-commands input modify-memory)]
    (reduce-kv #(+ %1 %3) 0 mem-cells)))

(part1 input)

(defn apply-mask-bit-to-addresses
  [addresses bit-idx bit]
  (case bit
    \0 addresses
    \1 (map #(bit-set % bit-idx) addresses)
    \X (->> addresses
            (map #(vector (bit-clear % bit-idx) (bit-set % bit-idx)))
            flatten)))

(defn get-address-variations
  [mask address]
  (let [addresses [address]]
    (reduce-kv apply-mask-bit-to-addresses addresses mask)))

(defn insert-at-all-variations
  [address value mask mem-cells]
  (let [variations (get-address-variations mask address)]
    (reduce #(assoc %1 %2 value) mem-cells variations)))

(defn part2
  [input]
  (let [mem-cells (process-commands input insert-at-all-variations)]
    (reduce-kv #(+ %1 %3) 0 mem-cells)))
