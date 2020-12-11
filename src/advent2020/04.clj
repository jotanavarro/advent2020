(ns advent2020.04)

;; https://adventofcode.com/2020/day/4

(def text-input (slurp "resources/input-04.txt"))

(def fields (set `("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))
(def fields-as-keywords `(:byr :eyr :pid :hgt :iyr :ecl :hcl))

(defn parse-to-lines
  "Will split the files \"passport\" data."
  [input]
  (clojure.string/split input #"\n\n"))

(defn identify-fields
  "Splits each field into an array element."
  [input]
  (clojure.string/split input #"[ | \n]"))

(defn trim-fields
  "Extracts a set with the fields of a passport"
  [input]
  (into #{} cat (map #(clojure.string/split % #":.*$") input)))

(defn correct-fields?
  [passport]
  (empty? (clojure.set/difference fields (trim-fields passport))))

(defn calculate
  [input val-fun]
  (let [passports (parse-to-lines input)]
    (loop [head (identify-fields (first passports))
           tail (rest passports)
           valid (if (val-fun head) 1 0)]
      (if (empty? tail)
        valid
        (let [n-head (identify-fields (first tail))
              n-tail (rest tail)
              n-valid (if (val-fun n-head) (inc valid) valid)]
          (recur n-head n-tail n-valid))))))

(defn all-valid?
  [passport]
  (let [mapify (fn [x]
                 (dissoc (reduce #(merge %1 {(keyword (first %2)) (second %2)})
                         {}
                         (map #(clojure.string/split % #":") x)) :cid))
        mapped-passport (mapify passport)]
    (and (= (sort fields-as-keywords) (sort (keys mapped-passport)))
         (let [byr (Integer/parseInt (mapped-passport :byr))]
           (>= 2002 byr 1920))
         (let [iyr (Integer/parseInt (mapped-passport :iyr))]
           (>= 2020 iyr 2010))
         (let [eyr (Integer/parseInt (mapped-passport :eyr))]
           (>= 2030 eyr 2020))
                  (re-matches #"^[\da-f]{6}$" (mapped-passport :hcl))
         (re-matches #"^\d{9}$")
         (clojure.set/subset? #{(mapped-passport :ecl)}
                              #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
         (let [[_ value unit] (re-matches #"^(\d{2,3})(cm|in)$" (mapped-passport :hgt))]
           (print "Last!")
           (and value unit (or (and (= unit "cm") (> 193 (Integer/parseInt value) 150))
                               (and (= unit "in") (> 76 (Integer/parseInt value) 59))))))))
