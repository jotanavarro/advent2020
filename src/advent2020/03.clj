(ns advent2020.03)

;; https://adventofcode.com/2020/day/3

(def text-input (slurp "resources/input-03.txt"))

(def text-lines (clojure.string/split-lines text-input))

(defn slide
  [rows]
  ((frequencies
    (loop [current (first rows)
           remains (rest rows)
           pos-x 0
           path (list (first current))]
      (if (empty? remains)
        path
        (let [head (first remains)
              tail (rest remains)
              acc-x (+ pos-x 3)
              acc-path (concat path (list (nth (cycle head) acc-x)))]
          (recur head tail acc-x acc-path)))))
   \#))

(defn slide-2
  [rows right down]
  ((frequencies
    (loop [current (first rows)
           remains (drop down rows)
           pos-x 0
           path (list (first current))]
      (if (empty? remains)
        path
        (let [head (first remains)
              tail (drop down remains)
              acc-x (+ pos-x right)
              acc-path (concat path (list (nth (cycle head) acc-x)))]
          (recur head tail acc-x acc-path)))))
   \#))

;; This returns the result of the second problem.
(def result-2 (* (slide-2 text-lines 1 1)
                 (slide-2 text-lines 3 1)
                 (slide-2 text-lines 5 1)
                 (slide-2 text-lines 7 1)
                 (slide-2 text-lines 1 2)))
