(ns advent2018.day11
  (:require [instaparse.core :as instaparse]))

;; this code is ugly, with zero cleanup beyong the raw "make it work"
;; on the night of.

(defn hundreds-digit [n]
  (- (int (nth (reverse (format "00%d" n)) 2))
     (int \0)))

(defn power-level [[x y] serial-number]
  (let [rack-id (+ x 10)
        power-base (* (+ (* rack-id y) serial-number)
                      rack-id)
        power (hundreds-digit power-base)]
    (- power 5)))

(defn total-power [[x y] serial-number size]
  (reduce +
          (for [x (range x (+ x size))
                y (range y (+ y size))]
            (power-level [x y] serial-number))))


(defn part1 [serial-number]
  (apply max-key
         second
         (for [x (range 1 298)
               y (range 1 298)]
           [[x y] (total-power [x y] serial-number 3)])))

;; ----------------------------------------
(defn make-grid [serial-number]
  (vec
   (for [x (range 0 301)]
     (vec
      (for [y (range 0 301)]
        (power-level [x y] serial-number))))))


(defn total-power-grid [grid [x y] size]
  (reduce +
          (for [x (range x (+ x size))
                y (range y (+ y size))]
            (get-in grid [x y]))))


(defn pick-best [results]
  (if (seq results)
    (apply max-key second results)
    [nil Integer/MIN_VALUE]))


(defn part2 [serial-number]
  (let [max-size 300
        grid (make-grid serial-number)]
    (pick-best
     (for [x (range 1 (dec max-size))
           y (range 1 (dec max-size))]
       (do
         ;; this is embarassing - if you see this, I apologize...
         (def tpg
           (memoize (fn [size]
                      (if (= size 1)
                        (get-in grid [x y])
                        (apply +
                               (tpg (dec size))
                               (get-in grid [(+ x (dec size)) (+ y (dec size))])
                               (for [n (range 0 (dec size))]
                                 (+ (get-in grid [(+ x n) (+ y (dec size))])
                                    (get-in grid [(+ x (dec size)) (+ y n)]))))))))
         (let [square-limit 25
               sizes
               (into [] (for [size (range 3 (min square-limit (- max-size x) (- max-size y)))]
                          [[x y size] (tpg size)]))]
           (pick-best sizes)))))))
