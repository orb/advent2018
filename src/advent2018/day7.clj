(ns advent2018.day7
  (:require [instaparse.core :as instaparse]))

;;----------------------------------------
(def grammar (instaparse/parser "
    lines = line (<NL> line)* <NL>?
    line = <'Step '> step <' must be finished before step '> step <' can begin.'>
    <step> = #'[A-Z]+'
    NL = '\n'
"))

(def transform {:lines vector
                :line vector})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

;; ----------------------------------------

(defn no-deps? [deps node]
  (every? #(not= node (second %)) deps))

(defn remove-dep [deps node]
  (remove #(= node (first %)) deps))

(defn next-work [deps nodes]
  (first (sort (filter #(no-deps? deps %) nodes))))


(defn part1 [input]
  ;; deps are the current active node dependencies
  ;; nodes are the nodes we've yet tto vistt
  ;; result are the nodes we have visited
  ;;
  ;; on each iteration
  ;; - pick the smallest node we haven't visted that has nothing depending on it
  ;; - remove it from work and add it to done
  ;; - remove all deps where another node is waiting on this node
  ;; - if there are no nodes we can work on, end
  ;;
  ;; This can probably be done without tracking the nodes needing work or
  ;; removing them from deps.
  (loop [deps (parse input)
         nodes (set (apply concat deps))
         result []]
    (if-let [next-node (next-work deps nodes)]
      (recur (remove-dep deps next-node)
             (disj nodes next-node)
             (conj result next-node))
      (apply str result))))

;; ----------------------------------------

(defn node-cost [node]
  (- (int (first node))
     64))

(defn n-of [n item]
  (into [] (take n (repeatedly (constantly item)))))

(defn part2 [input max-workers base-cost]
  ;; For part2, the loop is for each worker and step is incremented after we've
  ;; looped over each work. Maybe could be more readable with an inner loop over the workers?
  ;;
  ;; This is messy as I compensated for bugs without re-addressing the overall structure.
  ;; For example, on the first pass I didn't track which item each worker was working on, only the time
  ;; remaining. That doesn't work because we can't remove the node from the work list until it's done.
  ;; However, that left me with two seperate loop variables tracking the work, which seems uglier
  ;; than having one structure that tracks the work item and the step it will be done on.

  (loop [deps (parse input)
         nodes (set (apply concat deps))
         steps 0 ;; the current second we are on
         worker 0 ;; the current worker we are on
         timer (n-of max-workers 0) ;; time remaining for each worker
         work (n-of max-workers nil)] ;; the current work item for each worker
    (if (== worker max-workers)

      ;; all workers processed, end the step
      (if (and (every? zero? timer)
               (every? nil? work))
        steps ;; done
        (recur deps nodes (inc steps) 0 timer work))

      ;; do work for a the current worker
      (let [working-on (nth work worker)
            time-remaining (nth timer worker)]
        (if (= 0 time-remaining) ;; need work
          (if-let [next-node (next-work (remove-dep deps working-on) nodes)]
            (do ;; have work for this worker
              ;; note that next-node had to remove-dep on the current work item. this was an annoying bug
              (recur (remove-dep deps working-on) ;; remove the deps of the item we just finished
                     (disj nodes next-node) ;; remove the work we are starting from the work list
                     steps
                     (inc worker)
                     (assoc timer worker (+  (node-cost next-node) base-cost -1))
                     (assoc work worker next-node)))
            (do ;; no work available for this worker
              (recur (remove-dep deps working-on) ;; remove the deps for the item we finsihed
                    nodes
                    steps
                    (inc worker)
                    timer
                    (assoc work worker nil))))
          (do ;; keep working on job
            (recur deps nodes steps (inc worker) (update timer worker dec) work)))))))
