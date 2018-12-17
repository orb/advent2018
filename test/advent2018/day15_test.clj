(ns advent2018.day15-test
  (:require [advent2018.day15 :refer :all]
            [clojure.test :refer :all]
            [clojure.java.io :as io]))

(def samples [
"#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######
"

"#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######"

"#######
#E..EG#
#.#G.E#
#E.##E#
#G..#.#
#..E#.#
#######"

"#######
#E.G#.#
#.#G..#
#G.#.G#
#G..#.#
#...E.#
#######"

"#######
#.E...#
#.#..G#
#.###.#
#E#G#G#
#...#G#
#######"

"#########
#G......#
#.E.#...#
#..##..G#
#...##..#
#...#...#
#.G...G.#
#.....G.#
#########"
])

(def my-input (slurp (io/resource "day15-input.txt")))

(deftest test-part1
  (is (= 27730 (part1 (samples 0))))
  (is (= 36334 (part1 (samples 1))))
  (is (= 39514 (part1 (samples 2))))
  (is (= 27755 (part1 (samples 3))))
  (is (= 28944 (part1 (samples 4))))
  (is (= 18740 (part1 (samples 5))))
  (is (= 190777 (part1 my-input))))


(deftest test-part2
  (is (= 4988  (part2 (samples 0))))
  (is (= 31284 (part2 (samples 2))))
  (is (= 3478  (part2 (samples 3))))
  (is (= 6474  (part2 (samples 4))))
  (is (= 1140  (part2 (samples 5))))
  (is (= 47388 (part2 my-input))))
