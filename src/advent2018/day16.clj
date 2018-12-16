(ns advent2018.day16
  (:require [instaparse.core :as instaparse]))

;; I wrote eval-opcode before realizing the obvious situation that we might want to dispatch
;; on the opcode number and not just opcode symbol. I'm not sure if I would do it different now,
;; but I don't like the way I have to rewrite the instructions when mapping between number
;; and symbol.

;;----------------------------------------
;; parser
;;
;; The input format today was a little odd, so today's parser is a bit backy

(def grammar
  (instaparse/parser "
    all = inputs <NL> <NL> instructions
    inputs = (input <NL>)*
    input = before <NL> instruction <NL> after <NL>
    <before> = <'Before:'> <SP*> registers
    <after> = <'After:'> <SP*> registers
    instruction = number <SP> number <SP> number <SP> number
    registers = <'['> number <COMMA> <SP> number <COMMA> <SP> number <COMMA> <SP> number <']'>
    instructions = (instruction <NL>)+
    COMMA = ','
    SP = #' '
    number=#'[0-9]+'
    NL = '\n'
"))

(def transform {:all (fn [inputs instructions]
                       {:inputs inputs
                        :instructions instructions})
                :inputs vector
                :input vector
                :registers vector
                :instruction vector
                :instructions vector
                :number #(Integer/parseInt %)})

(defn parse [input]
  (instaparse/transform transform (instaparse/parse grammar input)))

;; ----------------------------------------
(def opcodes [:addr
              :addi
              :mulr
              :muli
              :banr
              :bani
              :borr
              :bori
              :setr
              :seti
              :gtir
              :gtri
              :gtrr
              :eqir
              :eqri
              :eqrr])

;; Maybe eval-opcode should take in an opcode to instruction mapping?
(defmulti eval-opcode
  (fn [[code a b c] [r0 r1 r2 r3]] code))

(defmethod eval-opcode :addr [[_ a b c] registers]
  (assoc registers c (+ (registers a) (registers b))))
(defmethod eval-opcode :addi [[_ a b c] registers]
  (assoc registers c (+ (registers a) b)))
(defmethod eval-opcode :mulr [[_ a b c] registers]
  (assoc registers c (* (registers a) (registers b))))
(defmethod eval-opcode :muli [[_ a b c] registers]
  (assoc registers c (* (registers a) b)))
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

;; ----------------------------------------

(defn try-opcodes
  "try all opcodes, except those in the except set"
  ([instruction]
   (try-opcodes instruction #{}))
  ([[before instruction after] except]
   (for [opcode opcodes
         :when (not (except opcode))
         :let [try-instruction (assoc instruction 0 opcode)]
         :when (= after (eval-opcode try-instruction before))]
     opcode)))

(defn part1 [inputs]
  (count
   (for [input inputs
         :let [matches (try-opcodes input)]
         :when (>= (count matches) 3)]
     input)))

;; ----------------------------------------

(defn find-mapping
  "determine all 16 mappings by looking for test inputs that could only
  be one input and reducing thing input size, hoping more opcodes
  resolve. Assumes there is a single unambigous mapping that can be determined."
  [inputs]
  (loop [known {}]
    (if (= 16 (count known))
      known
      (let [new-knowns
            (set (for [input inputs
                       :let [opcode (first (second input))]
                       :when (not (known opcode))
                       :let [possible (try-opcodes input (set (vals known)))]
                       :when (= 1 (count possible))]
                   ;; I shouldn't need to call try-opcodes over and over,
                   ;; but it's cheap and easy, so ...
                   {opcode (first possible)}))]
        (recur (reduce into known new-knowns))))))

(defn run-instructions
  "run the instructions given an opcode mapping. I don't see where it
  was specified that the registers should start out zero, but I made
  that assumption and it worked out"
  [mapping instructions]
  (loop [instructions instructions
         registers [0 0 0 0]]
    (if (empty? instructions)
      registers
      (let [mapped-instruction
            (update (first instructions) 0 mapping)]
        (recur (rest instructions)
               (eval-opcode mapped-instruction registers))))))

(defn part2 [inputs instructions]
  (let [registers (run-instructions (find-mapping inputs) instructions)]
    (registers 0)))
