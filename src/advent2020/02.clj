(ns advent2020.02)

;; https://adventofcode.com/2020/day/2

(def text-input (slurp "resources/input-02.txt"))

(def row-keys [:minimum :maximum :letter :password])

(defn parse
  "Transforms the input into a list of maps, like for example
  '1-2 c: foobar' into:
  {:min \"1\" :max \"2\" :letter \"c\" :password \"foobar\"}"
  [string]
  (map #(as-> % s
          (clojure.string/split s #"-| |: ")
          (zipmap row-keys s))
       (clojure.string/split-lines string)))

(defn validate
  "Assuming a valid row-key validates its contents."
  [row]
  (let [{:keys [minimum maximum letter password]} row
        freq-map (frequencies password)
        char-letter (first letter)]
    (and (contains? freq-map char-letter)
         (let [frequency (freq-map (first letter))]
           (and (>= frequency (Integer/parseInt minimum))
                (<= frequency (Integer/parseInt maximum)))))))

(defn new-validate
  "Validates a row given the rules of the second exercise"
  [row]
  (let [{p1 :minimum, p2 :maximum, l :letter, pwd :password} row
        pos1 (dec (Integer/parseInt p1))
        pos2 (dec (Integer/parseInt p2))]
    (and (< pos2 (count pwd))
         (let [char1 (nth pwd pos1)
               char2 (nth pwd pos2)
               charl (first l)]
           (and (or (= charl char1)
                    (= charl char2))
                (not (= char1 char2)))))))

(defn inspect-passwords
  [col fun]
  (count (filter fun (parse col))))
