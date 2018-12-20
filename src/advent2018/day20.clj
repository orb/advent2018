(ns advent2018.day20
      (:require [instaparse.core :as instaparse]
              [advent2018.day20-inputs :as inputs]))

;; My approach was to parse the path and search using a collection of
;; [point route] pairs. At each search iteration, I select an explored
;; node, and move it forward, creating multiple search points in the
;; case of alternation. Unfortunately, the input pattern is a bit
;; degenerate to be a bit smarter about not revisiting the same point
;; with the same pattern. (the way I do that is very inefficient, but
;; it still beats duplciating search paths)
;;
;; I thought my instaparsed pattern would be a clear win, but looking
;; at other solutions, simply parsing the input character by character
;; and maintaining a stack was a better solution
;;
;; Also, as I'm writing these notes, it seems I'm actually on tracking
;; doors in one direction from point1 to point2. It seems like this
;; should cause failures in computing the shortest paths, since point2
;; to point 1 cannot be traversed.  It happens to work out when
;; searching paths from the origin point, but I'd need to fix that
;; for other paths

;; ----------------------------------------

(def grammar
  (instaparse/parser "
    route = <'^'> pattern <'$'>
    pattern = (path | <'('> split <')'>)*
    path = #'[NSEW]*' | Epsilon
    split = pattern (<'|'> pattern)*
"))

(def transform {:route identity})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

;; ----------------------------------------

(defn move [[x y] dir]
  (cond
    (= dir \N)  [x (inc y)]
    (= dir \S)  [x (dec y)]
    (= dir \W)  [(dec x) y]
    (= dir \E)  [(inc x) y]))

(defn advance-path [pos path]
  (reduce move pos path))

(defn add-doors [pos path doors]
  (if (empty? path)
    doors
    (let [next-pos (move pos (first path))]
      (recur next-pos (rest path) (conj doors [pos next-pos])))))

(defn follow [in-routes doors]
  ;; the naming here turned out to be really confusing
  (loop [search-routes in-routes ;; pairs of [pos route] yet to explore
         doors doors             ;; set of [pos pos] indicating a door from pos1-> pos2
         seen-from-pos #{}]      ;; set of seen [pos route] so we don't redo work
    (if-let [current-route (first search-routes)]
      ;; if we've been down this route before, move on as there's no new doors to discover
      (if (seen-from-pos current-route)
        (recur (rest search-routes)
               doors
               seen-from-pos)

        ;; this is a new route,
        (let [[my-pos [route-type & route-patterns]] current-route]
          (if-let [first-pattern (first route-patterns)]
            (cond
              (= :path (first first-pattern)) ;; handle simple path
              (let [path (second first-pattern)
                    move-to (advance-path my-pos path)
                    next-doors (add-doors my-pos path doors)
                    remaining-pattern (into [:pattern] (rest route-patterns))]
                ;; ugg - duplicating the work of moving and adding the doors for
                ;; the path. maybe it would have been better to step one step
                ;; at a time and not try to be clever.
                (recur (conj (rest search-routes)
                             [move-to remaining-pattern])
                       next-doors
                       (conj seen-from-pos current-route)))

              (= :split (first first-pattern)) ;; handle alternation
              (recur (into (rest search-routes)
                           (for [pattern (rest first-pattern)]
                             [my-pos (into pattern (rest route-patterns))]))
                     doors
                     (conj seen-from-pos
                           current-route)))

            ;; current pattern is empty - no work
            (recur (rest search-routes)
                   doors
                   (conj seen-from-pos current-route)))))
      doors)))


(defn part1 [input]
  (let [doors (follow [[[0 0] (parse input)]]
                      #{})
        rooms (into #{[0 0]}
                    (map second doors))]
    (loop [n 0
           seen #{[0 0]}]
      (if (= seen rooms)
        n
        (recur (inc n)
               (into seen (for [[from to] doors
                                :when (seen from)]
                            to)))))))



;; ----------------------------------------
(defn part2-count [doors]
  (let [rooms (into #{[0 0]}
                    (map second doors))]
    (loop [n 0
           seen #{[0 0]}
           big-rooms 0]
      (if (= seen rooms)
        big-rooms
        (let [new-rooms
              (for [[from to] doors
                    :when (and (seen from)
                               (not (seen to)))]
                to)]
          (recur (inc n)
                 (into seen new-rooms)
                 (if (>=  (inc n) 1000)
                   (+ big-rooms (count new-rooms))
                   big-rooms)))))))
(defn part2 [input]
  (let [doors (follow [[[0 0] (parse input)]]
                      #{})]
    (part2-count doors)))

