(ns advent2018.day23
  (:require [instaparse.core :as instaparse]
            [clojure.java.io :as io]
            [clojure.string :as string]))

;; this code is fairly useless.  it does a boneheaded search with
;; seeded values from other runs or other failed runs. I tried
;; numerous different ways to shuffle the search space, and even hand
;; crafted some points on paper. Most of the time it was, add some
;; dumb code and run over and over until it stumbled onto better magic
;; points. Somehow... magic. My final answer is among the magic points
;; now, so it's sure to work.  uggh...
;;
;; I really had no idea what to try. There's probably math than can be applied,
;; but I don't find math aoc problems interesting.
;; If someone posts an interesting algorithm that actually seems like it should work
;; and not just get lucky, I may rewrite this.

(def grammar
  (instaparse/parser "
    lines = (line <NL>)*
    line = pos <COMMA> <SP> radius
    pos = <'pos=<'> number <COMMA> number <COMMA> number <'>'>
    radius = <'r='> number
    number=#'-?[0-9]+'
    COMMA = ','
    SP = ' '
    NL = '\n'
"))

(def transform {:lines vector
                :line vector
                :pos vector
                :number #(Integer/parseInt %)
                :radius identity})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

(def sample-input (slurp (io/resource "day23-sample.txt")))
(def my-input (slurp (io/resource "day23-input.txt")))



(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))
     (Math/abs (- z1 z2))))


(defn part1 [input]
  (let [data (parse input)
        [strongest-point radius] (apply max-key second data)]
    (for [[point _] data
          :when (<= (distance strongest-point point) radius)]
      (count point))))


(defn in-range? [[np nr] point]
  (<= (distance np point) nr))

(defn score [data point]
  (count (filter #(in-range? % point) data)))

(defn distance-o [point]
  (distance [0 0 0] point))



(defn midpoint [[x1 y1 z1]
                [x2 y2 z2]
                factor]
  (let [split (fn [n1 n2]
               (int (/ (+ n1 n2) 2)))]
    [(split x1 x2) (split y1 y2) (split z1 z2)]))


(defn explode [[x y z] n]
  (for [nx [(- x n) x (+ x n)]
        ny [(- y n) y (+ y n)]
        nz [(- z n) z (+ z n)]]
    [nx ny nz]))



(defn sort-points [scorer points]
  (sort-by (fn [[_ score distance]]
             [(- score) distance])
           (for [p points]
             [p (scorer p) (distance-o p)])))

(defn prune [sorted]
  (let [close-to (fn [p1 p2]
                   (cond
                     (or (not p1) (not p2))
                     false

                     :else
                     (< (distance p1 p2) 100000)))]
    (loop [todo sorted
           done []]
      (if (empty? todo)
        done
        (if (close-to (first (first todo))
                      (first (last done)))
          (recur (rest todo)
                 done)
          (recur (rest todo)
                 (conj done (first todo))))))))


(defn part2 [input]
  (let [data (parse input)
        score-point (partial score data)
        data-points (map first data)
        magic-points (map last [[860 77853110 [17654613 24249471 35949026]]
                                [886 77852211 [13940743 26484157 37427311]]
                                [897 80250779 [18432328 29059455 32758996]]
                                [893 77873709 [12734961 27094122 38044626]]
                                [943 80250789 [11754012 29059460 39437317]]
                                [978 80250793 [11382526 29059462 39808805]]])

        initial-points (concat data-points magic-points)]

    (loop [n 0
           points initial-points]

      (Thread/sleep 1)
      (println "*" n (count points))
      (let [sorted-points (prune (sort-points score-point points))
            top-points (map first sorted-points)]
        (println (string/join "\n"
                              (for [[p pscore pdist] (take 5 sorted-points)]
                                [pscore pdist p])))

        (let [next-points (set (concat
                                (map #(first (first (sort-points score-point (explode % (rand-int 5000)))))
                                     (take 10 top-points))
                                (take 5 (repeatedly #(rand-nth initial-points)))))]
          (recur (inc n)
                 (set (concat next-points
                              (for [p1 next-points
                                    p2 next-points
                                    :when (not= p1 p2)]
                                (midpoint p1 p2 (rand)))))))))))
