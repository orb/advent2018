(ns advent2018.day12-test
  (:require [advent2018.day12 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))


  (def sample-input "initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #")
(def my-input (slurp (io/resource "day12-input.txt")))


(deftest test-part1
  (is (= 1672 (part1 sample-input))))

(deftest test-part2
  (is (= 1650000000055 (part2 sample-input))))
