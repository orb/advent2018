(ns advent2018.day12
  (:require [instaparse.core :as instaparse]))

;; another ugly solution. I don't mind optimization problems, but I
;; hate problems where participants are encouraged to just hack around
;; with their specific input. Do all sequences converge? Do they all
;; converge like this? Does this code work on anything but the crafted
;; input we were given? Who knows? Who care?
;;
;; given my extreme lack of excitement for this problem, along with my
;; lack of time, I'm choosing not to clean up this first pass solution
;; at all.

;;----------------------------------------
(def grammar
  (instaparse/parser "
    lines = initial <NL>
            <NL>
            rule (<NL> rule)* <NL>?
    <initial> = <'initial state: '> encoded
    encoded = hashdot *
    <hashdot> = '#' | '.'
    rule = encoded <' => '> hashdot
    NL = '\n'
"))

(def transform {:lines (fn [state & rules]
                         {:state state
                          :rules (into {} rules)})
                :rule vector
                :encoded #(apply str %&)
                :xencoded vector})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

;; ----------------------------------------

(defn as-str [chars]
  (apply str chars))


(defn count-score [state start]
  (loop [state state
         pos start
         score 0]
    (if (seq state)
      (recur (rest state)
             (inc pos)
             (+ score (if (= \# (first state))
                        pos
                        0)))
      score)))


(defn normalize [state start]
  (let [[_ leading middle trailing]
        (re-matches #"(\.*)(\#.*\#)(.*)" state)]
    [middle (+ start (count leading))]))

(defn run [state rules iterations]
  (loop [state state
         n 0
         start 0
         prior ["" 0]]
    (let [[normal normal-start] (normalize state start)]
      #_(when (= 0 (mod n 5000))
          (println "*" n normal))

      (if (>= n iterations)
        (count-score state start)
        (if (= (first prior) normal)
          (do
            #_(println "== DUP" n normal-start (second prior))
            #_(println "=>" normal)
            (recur state
                   iterations
                   (+ start (- iterations n))
                   nil))
          (let [next-state
                (as-str
                 (for [position (map as-str (partition 5 1 (format "...%s..." state)))]
                   (if-let [replacement (rules position)]
                     replacement
                     ".")))]
            (recur next-state
                   (inc n)
                   (- start 1)
                   [normal normal-start])))))))


(defn part1 [input]
  (let [parsed (parse input)]
    (run (:state parsed)
      (:rules parsed)
      20)))

(defn part2 [input]
  (let [parsed (parse input)]
    (run (:state parsed)
      (:rules parsed)
      50000000000)))
