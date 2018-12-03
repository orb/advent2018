(ns advent2018.day2-test
  (:require [advent2018.day2 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-lines [text]
  (string/split text #"\n"))

(def sample-input1
  ["abcdef"
   "bababc"
   "abbcde"
   "abcccd"
   "aabcdd"
   "abcdee"
   "ababab"])

(def sample-input2
  ["abcde"
   "fghij"
   "klmno"
   "pqrst"
   "fguij"
   "axcye"
   "wvxyz"])

(def my-input (read-lines (slurp (io/resource "day2-input.txt"))))


(deftest test-count-trues
  (is (= 0 (count-trues [])))
  (is (= 1 (count-trues [false false true false])))
  (is (= 0 (count-trues [false false])))
  (is (= 3 (count-trues [true true false false true]))))

(deftest test-make-count
  (is (= [false false] (make-count "abc")))
  (is (= [false false] (make-count "aaaaabbbbbbccccccaaaa")))
  (is (= [true false] (make-count "abcaxxxxxcccccc")))
  (is (= [false true] (make-count "abcabcabc")))
  (is (= [true true] (make-count "cbacabc"))))

(deftest test-remove-differing
  (is (empty? (remove-differing [1 2 3] [])))
  (is (empty? (remove-differing [1 2 3] [4 5 6])))
  (is (= [1 2 3] (remove-differing [1 2 3] [1 2 3])))
  (is (= [1 2 3] (remove-differing [1 2 3] [1 2 3 4 5 6])))
  (is (= [1 2 3 4] (remove-differing [1 2 3 4 6 5] [1 2 3 4 5 6])))
  (is (= [1 4] (remove-differing [1 2 4] [1 3 4]))))

(deftest test-part1
  (is (= 12 (part1 sample-input1)))
  (is (= 8892 (part1 my-input))))

(deftest test-part2
  (is (= "fgij" (part2 sample-input2)))
  (is (= "zihwtxagifpbsnwleydukjmqv" (part2 my-input))))

