(ns advent2018.day22)

;; at last, a fun one!
;;
;; calculating erosion needs a dynamic programming kind of solution
;; I solved that by memoizing the function, that makes the code really simple, but it's
;; also rather ugly. It's hard to memoize recursive calls without defn mutation. I opted
;; for the fast route here.
;;
;; With erosion/terrain known, the final task is a search task. I chose a
;; weighted BFS (basically djikstra) approach. Since the target y is much
;; greater than the target-x, this search tended to wander far over into
;; x values that were unlikely to be in final path. A* would solve that, but
;; that would require a little more code. As a first pass I simply bounded
;; the search a small distance outside the start/end rectangle. (based on a guess
;; of how much would be needed for the input) This is very poor heuristic, but it worked
;; for night-of. I'll replace it with a more robust heuristic later.

;; my inputs. due to the use of memos thhat need the input, it was easier not
;; separate out the inputs
(def depth 5355)
(def target-x 14)
(def target-y 796)

(comment (def depth 510)
         (def target-x 10)
         (def target-y 10))

(def erosion
  "memoized erosion function, uses global state, so it's rather ugly. It turns out that writing
     a memoized un-def'd function isn't so easy. For speed purposes,"
  (memoize
   (fn [x y]
     (let [geologic
           (cond
             (or (= x y 0)
                 (and (= x target-x) (= y target-y)))
             0

             (= y 0)
             (* x 16807)

             (= x 0)
             (* y 48271)

             :else
             (* (erosion x (dec y))
                (erosion (dec x) y)))]
       (mod (+ geologic depth) 20183)))))

(defn risk
  "the risk 0,1,2 of the x,y as dettermined by the erosion level"
  [x y]
  (mod (erosion x y) 3))


(defn part1 []
  (reduce +
          (for [x (range (inc target-x))
                y (range (inc target-y))]
            (risk x y ))))



;; ----------------------------------------

(defn terrain
  "the terrain, as influenced by the global state"
  [x y]
  (nth [:rocky :wet :narrow] (risk x y)))

(defn print-map
  "print out the terrian using the global parameters"
  []
  (doseq [y (range (inc target-y))]
    (println (apply str (for [x (range (inc target-x))]
                          (case (terrain x y)
                            :rocky  \.
                            :wet    \=
                            :narrow \|))))))


(defn valid? [[x y gear :as pos]]
  "test if an x/y/gear combo is valid. Note the arbitrary x/y constrained which
   let be first pass djikstra work quickly. This is kind of bad."
  (and pos
       (<= 0 x (+ target-x 30))
       (<= 0 y (+ target-y 30))
       (case gear
         :torch
         (#{:rocky :narrow} (terrain x y))
         :climbing-gear
         (#{:rocky :wet} (terrain x y))
         :none
         (#{:wet :narrow} (terrain x y)))))

(defn neighbors [[x y gear]]
  "find all the neighbors reachable from a given pos/gear combo"
  (filter valid? [(when (not= gear :torch)
                    [x y :torch])
                  (when (not= gear :climbing-gear)
                    [x y :climbing-gear])
                  (when (not= gear :none)
                    [x y :none])
                  [x (inc y) gear]
                  [x (dec y) gear]
                  [(dec x) y gear]
                  [(inc x) y gear]]))

(defn move-cost
  "compute th cost of moving from p1 to p2"
  [[x1 y1 gear1 :as p1]
   [x2 y2 gear2 :as p2]]
  (if (= gear1 gear2)
    1
    7))

;; given a map of pairs or [move cost] move the pair
;; with the lowest cost, without regard to ties.
(defn cheapest
  "given a map of pairs [move cost] return the pair
   with the lowest cost, with no defined tie breaker"
  [moves]
  (apply min-key second moves))


(defn part2 []
  (let [start [0 0 :torch]
        end   [target-x target-y :torch]]
    (loop [seen {}           ;; visited [pos cost]
           unseen {start 0}] ;; unvisited [pos cost], unordered
      (if-let [final-cost (seen end)]
        final-cost
        (let [[move-to move-to-cost] (cheapest unseen) ;; find any of cheapest places to visit

              ;; and all the places reachable from that that we haven't already visited
              next-moves (into {}
                               (for [neighbor (neighbors move-to)
                                     :when (not (seen neighbor))
                                     :let [neighbor-cost (+ move-to-cost
                                                            (move-cost move-to neighbor))]]
                                 [neighbor neighbor-cost]))]

          ;; add move-to [move cost] as visited and
          ;;
          ;; add all the newly visitable neighbors iff the new
          ;; distance is shorter than any known distance
          (recur (assoc seen move-to move-to-cost)
                 (merge-with min ;; uses the cheapest cost when a pos is on both sides
                             (dissoc unseen move-to) ;; also remove the visited node
                             next-moves)))))))


