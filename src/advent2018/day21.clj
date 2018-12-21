(ns advent2018.day21)

;; I dislike this kind of problem since it's the challenge is figuring out the
;; puzzle not coding. My compulsion to complete AoC is preventing me from skipping
;; this problem, even though I really didn't want to do it.
;;
;; I've made no attempts to clean this up, write tests or anything. All I did was translate the
;; input to state machine and ran it for the values. Ultimately the input is some sort
;; of seeded number generator. The code exits if R0 (what we are solving for) is in the sequence
;; and goes forever if it doesn't. So, the shortest path through is the first number in the sequence
;; and the longest terminating path through is the last non-repeating number in the sequence.
;;
;; in retrospect, I probably could have just re-used the elfcode interpretter here and not done
;; this dumb machine. It looks like there are about 12.4k unique values, so it wouldn't have taken
;; all that long. I also could have further optimized the generator here, but I'm done.


;; advent2018.day21> (time (so-dumb))
;; "Elapsed time: 127964.635402 msecs"
;; [15615244 12963935]

(defn so-dumb []
  (let [seen (volatile! [])] ;; I didn't even move this in the loop. That's how much I care.
    (loop [step 0
           registers [0 0 0 0 0 0]]
      (case step
        0 (recur 6 registers) ;; setup
        6 (recur 8 (assoc registers
                          4 (bit-or (registers 5) 65536)
                          5 15466939))
        8 (do
            #_(println "/8 " registers)
            (let [R3 (bit-and (registers 4) 255)
                  R5 (+ (registers 5) R3)
                  R5 (bit-and R5 16777215)
                  R5 (* R5 65899)
                  R5 (bit-and R5 16777215)
                  R3 (if (> 256 (registers 4)) 1 0)]
              (if (= R3 1)
                (recur 28 (assoc registers 3 R3 5 R5))
                (recur 17 (assoc registers 3 R3 5 R5)))))

        17 (do
             #_(println "/17" registers)
             (recur 18 (assoc registers 3 0)))

        18 (do
             #_(println "/18" registers)
             (let [R1 (inc (registers 3))
                   R1 (* R1 256)
                   R1 (if (> R1 (registers 4)) 1 0)]
               (if (= R1 1)
                 (recur 26 (assoc registers 1 R1))
                 (recur 24 (assoc registers 1 R1)))))

        24 (do
             #_(println "/24" registers)
             (recur 18 (update registers 3 inc)))

        26 (do
             #_(println "/26" registers)
             (recur 8 (assoc registers 4 (registers 3))))

        28 (let [current-value (registers 5)]
             #_(println "/28" registers)
             (if (some #(= current-value %) @seen)
               [(first @seen) (last @seen)]
               (do
                 (vswap! seen conj current-value)
                 #_(when (= 0 (mod (count @seen) 1000))
                     (println "..." (count @seen) current-value))
                 (recur 6 (assoc registers 3 0)))))))))


