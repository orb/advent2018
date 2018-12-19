(ns advent2018.day19
  (:require [instaparse.core :as instaparse]
            [advent2018.day19-inputs :as inputs]))

;; Ok - this is now the worst problem of 2018. Part 1 was fun, but
;; part 2 was not even a programming problem it was a puzzle in which
;; you might write some code to help you solve it. Maybe I should
;; just accept that's what aoc is about, but I hate it. So, like everyone
;; else did, I cheated. I didn't write code, I just looked for the magic number
;; and computed the magic value. dumb dumb dumb. Even worse than day 15, which
;; despite being a poor programming problem was still a programming problem.

(def grammar
  (instaparse/parser "
    all = ip <NL> instructions
    ip = <'#ip '> number
    instructions =  (instruction <NL>)+
    instruction = opcode <SP> number <SP> number <SP> number
    opcode = #'[a-z]+'
    COMMA = ','
    SP = #' '
    number=#'[0-9]+'
    NL = '\n'
"))

(def transform {:all (fn [ip instructions]
                       {:ip-register ip
                        :instructions instructions})
                :instructions vector
                :instruction vector
                :ip identity
                :opcode keyword
                :number #(Integer/parseInt %)})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

;; ----------------------------------------
(defmulti eval-opcode
  (fn [[code a b c] [r0 r1 r2 r3]] code))

(defmethod eval-opcode :addr [[_ a b c] registers]
  (assoc registers c (+' (registers a) (registers b))))
(defmethod eval-opcode :addi [[_ a b c] registers]
  (assoc registers c (+' (registers a) b)))
(defmethod eval-opcode :mulr [[_ a b c] registers]
  (assoc registers c (*' (registers a) (registers b))))
(defmethod eval-opcode :muli [[_ a b c] registers]
  (assoc registers c (*' (registers a) b)))
(defmethod eval-opcode :banr [[_ a b c] registers]
  (assoc registers c (bit-and (registers a) (registers b))))
(defmethod eval-opcode :bani [[_ a b c] registers]
  (assoc registers c (bit-and (registers a) b)))
(defmethod eval-opcode :borr [[_ a b c] registers]
  (assoc registers c (bit-or (registers a) (registers b))))
(defmethod eval-opcode :bori [[_ a b c] registers]
  (assoc registers c (bit-or (registers a) b)))
(defmethod eval-opcode :setr [[_ a b c] registers]
  (assoc registers c (registers a)))
(defmethod eval-opcode :seti [[_ a b c] registers]
  (assoc registers c a))
(defmethod eval-opcode :gtir [[_ a b c] registers]
  (assoc registers c (if (> a  (registers b)) 1 0)))
(defmethod eval-opcode :gtri [[_ a b c] registers]
  (assoc registers c (if (> (registers a) b) 1 0)))
(defmethod eval-opcode :gtrr [[_ a b c] registers]
  (assoc registers c (if (> (registers a) (registers b)) 1 0)))
(defmethod eval-opcode :eqir [[_ a b c] registers]
  (assoc registers c (if (= a  (registers b)) 1 0)))
(defmethod eval-opcode :eqri [[_ a b c] registers]
  (assoc registers c (if (= (registers a) b) 1 0)))
(defmethod eval-opcode :eqrr [[_ a b c] registers]
  (assoc registers c (if (= (registers a) (registers b)) 1 0)))


(defn set-ip-register [ip-register ip registers]
  (assoc registers ip-register ip))

(defn part1 [input]
  (let [{:keys [ip-register instructions]} (parse input)
        n (volatile! 0)]
    (loop [ip 0
           registers [0 0 0 0 0 0]]
      (if (and (>= ip 0) (< ip (count instructions)))
        (let [next-registers (eval-opcode (instructions ip)
                                          (set-ip-register ip-register ip registers))]
          (recur (inc (next-registers ip-register))
                 next-registers))
        registers))))


;; part 2 wasn't a coding problem, so there's no code
