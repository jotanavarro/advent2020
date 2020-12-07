(ns advent2020.01)

(def text-input (slurp "resources/input-01.txt"))

(defn str->int [s]
  (Integer/parseInt s))

;; https://adventofcode.com/2020/day/1

(defn calculate [col acc]
  (let [has (fn [elem arr] (some #(= elem %) arr))]
    (loop [current (first col)
           remains (rest col)]
      (let [target (str (- acc (str->int current)))]
        (if (has target remains)
          (list current target)
          (if (empty? remains)
            (list)
            (recur (first remains) (rest remains))))))))

(defn calc-three [col]
  (loop [current (first col)
         remains (rest col)]
    (let [searching (- 2020 (str->int current))
          result (calculate remains searching)]
      (if-not (empty? result)
        (print (merge result current))
        (if (empty? remains)
          (list)
          (recur (first remains) (rest remains)))))))
