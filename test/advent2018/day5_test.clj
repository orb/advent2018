(ns advent2018.day5-test
  (:require [advent2018.day5 :refer :all             ]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]))


(def sample-input "dabAcCaCBAcCcaDA")

(def my-input (string/trim (slurp (io/resource "day5-input.txt"))))

(deftest test-part1
  (is (= 10 (part1 sample-input)))
  (is (= 10584 (part1 my-input))))

(deftest test-part2
  (is (= 4 (part2 sample-input)))
  (is (= 6968 (part2 my-input))))
