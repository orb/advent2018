(ns advent2018.day20-test
  (:require [advent2018.day20 :as day20]
            [advent2018.day20-inputs :as inputs]
            [clojure.test :as t]))

(t/deftest test-part1
  (t/is (= 3 (day20/part1 "^WNE$" )))
  (t/is (= 10 (day20/part1 "^ENWWW(NEEE|SSE(EE|N))$" )))
  (t/is (= 18 (day20/part1 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")))
  (t/is (= 23 (day20/part1 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")))
  (t/is (= 3014 (day20/part1 inputs/my-input))))

(t/deftest test-part2
  (t/is (= 8279 (day20/part2 inputs/my-input))))
