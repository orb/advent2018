(ns advent2018.day10
  (:require [instaparse.core :as instaparse]))

;;----------------------------------------

;;
(def grammar (instaparse/parser "
    lines = line (<NL> line)* <NL>?
    line = <label> xy <SP> <label> xy
    label = #'[a-z]+='
    xy = <'<'> number <','> number <'>'>
    number = <SP*> #'-?[0-9]+' <SP*>
    SP = ' '+
    NL = '\n'
"))

(def transform {:lines vector
                :line vector
                :xy vector
                :number #(Integer/parseInt %)})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

;; ----------------------------------------
;; I wasn't willing to entertain OCR, so this code is really just for REPL use
;;
;; The code gets a little confused on when to uses points (a vector of XYs)
;; and stars, the starmap which is a vector of [point delta] records
;;
;; also I should probably writte guess so that it takes a starmap and not an input.
;; I'll save that for some day when I want to refactor it

(defn xrange [points]
  (let [xs (map first points)]
    (range (apply min xs)
           (inc (apply max xs)))))

(defn yrange [points]
  (let [ys (map second points)]
    (range (apply min ys)
           (inc (apply max ys)))))

(defn span
  "the distance spanned by an axis range"
  [ns]
  (- (last ns) (first ns)))

(defn find-area [stars]
  (let [points (map first stars)]
    (* (span (xrange points))
       (span (yrange points)))))

(defn display
  "display a starfield, only showing tthe rectangle of the sky that is signficant"
  [stars]
  (let [points (set (map first stars))
        xs (xrange points)
        ys (yrange points)]

    (doseq [y ys]
      (do
        (doseq [x xs]
          (if (points [x y])
            (print "*")
            (print " ")))
        (println)))))

(defn move-stars
  "moves the stars n steps, defaults to 1.
   I guess really I shouldn't carry the deltas along and just return the points"
  ([stars]
   (move-stars stars 1))
  ([stars n-steps]
   (for [[[x y] [dx dy]] stars]
     [[(+ x (* dx n-steps)) (+ y (* dy n-steps))]
      [dx dy]])))

(defn guess
  "under the assumption that the stars converge and diverge, search for the number
   of steps that minimizes the area of the star field"
  [input]
  (loop [n 0
         stars (parse input)
         area (inc (find-area stars))]
    (let [next-stars (move-stars stars)
          next-area (find-area next-stars)]
      (if (> next-area area)
        n
        (recur (inc n)
               next-stars
               next-area)))))

(defn run
  "run an input and display the stars at the guess"
  [input]
  (let [n (guess input)]
    (->  (parse input)
         (move-stars n)
         (display))))

