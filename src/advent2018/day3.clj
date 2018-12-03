(ns advent2018.day3
  (:require [instaparse.core :as instaparse]))

;; ----------------------------------------
;; parser - produces a [] of claim objects

(def grammar
  (instaparse/parser "
    claims = claim (<NL> claim)* <NL>?
    claim = <'#'> number <SP> <'@'> <SP> number <','> number <':'> <SP> number <'x'> number
    NL = '\n'
    SP = #' '
    number = #'[0-9]+'
"))

(def transform
  {:number #(Integer/parseInt %)
   :claim (fn [claim-id x y w h]
            {:id claim-id
             :x x
             :y y
             :w w
             :h h})
   :claims vector})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))


;; ----------------------------------------
;; map management - the map is a sparse map (uggh) of [x y] points to counts
;; if a point isn't in the map, it has no claims and it's count will be zero
;; It's not the most efficient representation, but initialization is free,
;; claimed points can be quickly iterated, and lookup is performant enough
;; for the task

(defn points-in-claim
  "generate all the [x y] points in a claim"
  [claim]
  (for [x (range (:x claim) (+ (:x claim) (:w claim)))
        y (range (:y claim) (+ (:y claim) (:h claim)))]
    [x y]))

(defn draw-point
  "draw a point into a map, incrementing the claim count at that point"
  [claim-map point]
  (update claim-map point (fnil inc 0)))

(defn draw-claim
  "draw all the points in a claim into a map"
  [claim-map claim]
  (reduce draw-point claim-map (points-in-claim claim)))


(def empty-claim-map {})

;; ----------------------------------------
(defn part1 [input]
  ;; draw the claim map and count the points with multtiple claims
  (let [claims (parse input)
        claim-map (reduce draw-claim empty-claim-map claims)
        overlapping (for [[point count] claim-map
                         :when (> count 1)]
                      point)]
    (count overlapping)))

(defn part2 [input]
  ;; draw the claim map, then iterate over the claims to find the
  ;; (first) one whose points all have a claim count of one
  (let [claims (parse input)
        claim-map (reduce draw-claim empty-claim-map claims)
        claimed-once? #(= 1 %)
        check-no-overlap (fn [claim]
                           ;; check if every point in the claim is claimed exactly once
                           (let [points (points-in-claim claim)
                                 claim-counts (map claim-map points)]
                             (every? claimed-once? claim-counts)))
        desired-claim (first (filter check-no-overlap claims))]
    (:id desired-claim)))
