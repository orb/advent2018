(ns advent2018.day16-test
  (:require [advent2018.day16 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def my-input (parse (slurp (io/resource "day16-input.txt"))))

(deftest test-try
  (is (= [:addi :mulr :seti]
         (try-opcodes [[3, 2, 1, 1] [9 2 1 2] [3, 2, 2, 1]]))))

(deftest test-part1 []
  (is (= 592 (part1 (:inputs  my-input)))))

(deftest test-part2 []
  (is (= 557 (part2 (:inputs  my-input)
                    (:instructions my-input)))))
