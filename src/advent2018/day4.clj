(ns advent2018.day4
  (:require [instaparse.core :as instaparse]))


;; ----------------------------------------
;; parser input
(def grammar
  (instaparse/parser "
    records = record (<NL> record)* <NL>?
    record = when <SP> event
    when = <'['> day <SP> time <']'>
    event = wake | sleep | start
    wake = <'wakes up'>
    sleep = <'falls asleep'>
    start = <'Guard #'> number <' begins shift'>
    NL = '\n'
    SP = ' '
    dash = '-'
    colon = ':'
    day = number <dash> number <dash> number
    time = number <colon> number
    number = #'[0-9]+'
  "))

(def transform {:number #(Integer/parseInt %)
                :records vector
                :record merge
                :day vector
                :time vector
                :when (fn [day time]
                        {:day day :time time})})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

;; ----------------------------------------
;; processing

(defn add-minutes-for-guard
  "add a seq of minutes to the guards sleep list"
  [guards guard-number minutes-range]
  (update guards guard-number concat minutes-range))

(defn process-records
  "process a sorted list of records, returning a map from guard number
  to a seq of minutes numbers they were asleep"
  [records]
  (loop [records records ;; loop over records
         guards {} ;; really bad name, the result accumulator
         ;; local iteration state
         current-guard nil
         asleep-since  nil]
    (if (empty? records)
      guards ;; exit condition
      (let [{:keys [day time event]} (first records)]
        (cond
          (= :start (first event))
          (recur (rest records)
                 (if asleep-since
                   (add-minutes-for-guard guards current-guard (range asleep-since 60))
                   guards)
                 (second event)
                 (if (= 0 (first time))
                   (second time)
                   0))

          (= :wake (first event))
          (let [minute (second time)]
            (recur (rest records)
                   (add-minutes-for-guard guards
                                          current-guard
                                          (range asleep-since (second time)))
                   current-guard
                   nil))

          (= :sleep (first event))
          (recur (rest records)
                 guards
                 current-guard
                 (second time)))))))

;; ----------------------------------------

(defn max-key-by
  "returns the key with the maximum value according to the sort fn.
   sort fn is called on the entire map entry, not the just the value -
   it looks like it would have been betttter to sort by only the value.
   also, probably could have just used max-key"
  [map-data sort-fn]
  (let [sorted (sort-by sort-fn map-data)
        [k v] (last sorted)]
    k))

(defn sleepiest-guard
  "find the guard the slept the most minutes"
  [guards]
  (max-key-by guards #(count (second %))))

(defn sleepiest-minute
  "given a seq of minutes, find the minute that occurs the most. The problem
   doesn't state how to handle when two minutes are the most, so I don't care her"
  [minutes]
  (max-key-by (frequencies minutes) second))

(defn part1 [input]
  (let [guards (process-records (sort-by (juxt :day :time) (parse input)))
        guard (sleepiest-guard guards)
        minute (sleepiest-minute (guards guard))]
    (* guard minute)))


;; ----------------------------------------

(defn part2 [input]
  (let [guards (process-records (sort-by (juxt :day :time) (parse input)))]
    (let [counts
          (for [[guard minutes] guards]
            (let [minute (sleepiest-minute minutes)
                  howmany ((frequencies minutes) minute)]
              [howmany (* guard minute)]))
          [howmany score] (apply max-key first counts)]
      score)))
