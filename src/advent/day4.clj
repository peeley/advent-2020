(ns advent.day4
  (:require [clojure.string :as str]))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid"})

(defn passport-fields-valid?
  [passport-fields]
  (= (conj passport-fields "cid") required-fields))

(defn get-fields-from-passport
  [passport]
  (into #{} (map #(first (str/split % #":")) (str/split passport #"[ \n]"))))

(defn part1
  [input]
  (count
    (filter passport-fields-valid?
      (map get-fields-from-passport input))))

; absolute monstrosity but i do not want to get
; sucked into a vortex writing a whole ass parser
(def field-regex
  {"byr" #"(19[2-9][0-9])|(200[012])"
   "iyr" #"(201[0-9])|(2020)"
   "eyr" #"(202[0-9])|(2030)"
   "hgt" #"(1(([5-8][0-9])|(9[0-3]))cm)|(((59)|(6[0-9])|(7[0-6]))in)"
   "hcl" #"#[0-9a-f]{6}"
   "ecl" #"(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)"
   "pid" #"[0-9]{9}"
   "cid" #".*"})

(defn all-passport-fields-valid?
  [passport]
  (let [passport-fields (str/split passport #"[ \n]")
        passport-key-vals (map #(str/split % #":") passport-fields)]
    (every?
     identity
     (map
      (fn match-regex
        [[field-name field-data]]
        (not (nil? (re-matches (get field-regex field-name) field-data))))
      passport-key-vals))))

(defn passport-contains-all-fields?
  [passport]
  (let [all-field-keys (set (keys field-regex))
        passport-fields (str/split passport #"[ \n]")
        passport-field-keys (set (map #(first (str/split % #":")) passport-fields))
        passport-key-vals-including-cid (conj passport-field-keys "cid")]
    (= all-field-keys passport-key-vals-including-cid)))

(defn passport-valid?
  [passport]
  (and
   (all-passport-fields-valid? passport)
   (passport-contains-all-fields? passport)))

(defn part2
  [input]
  (let [split-input (str/split input #"\n\n")]
    (count (filter passport-valid? split-input))))
