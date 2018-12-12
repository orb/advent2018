(ns advent2018.day11-test
  (:require [advent2018.day11 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def sample-input nil)
(def my-input 3463)


(deftest test-power
  (is (= 4 (power-level [3 5] 8)))
  (is (= -5 (power-level [122 79] 57)))
  (is (= 0 (power-level [217 196] 39)))
  (is (= 4 (power-level [101 153] 71))))


