(ns intcode.processor
  (:require [clojure.core.async :as async]
            [clojure.math.combinatorics :as comb]))

(defn- instruction->opcode-and-param-modes
  "Returns a seq of (opcode, param 1 mode, param 2 mode, param 3 mode..."
  [instruction]
  (cons (mod instruction 100)
        (->> (iterate #(/ % 10)
                      (-> instruction
                          (/ 100)
                          int))
             (map #(-> % int (mod 10))))))

(defn- read-tape
  "Reads the literal contents of the tape at a specific point."
  [tape ptr]
  (let [val (get tape ptr)]
    (if (nil? val)
      (throw (Exception. "Instruction pointer out of range."))
      val)))

(defn- get-data
  "Gets data at an address, or zero if that address is uninitialized."
  [tape addr]
  (get tape addr 0))

(defn- get-param-value
  "Gets a value from the tape depending on relative-base, operating mode, and ptr"
  [tape relative-base mode ptr]
  (case mode
    0
    (->> ptr
         (read-tape tape)
         (get-data tape))

    1
    (read-tape tape ptr)

    2
    (let [addr (+ (read-tape tape ptr) relative-base)]
      (get-data tape addr))))

(defn- get-addr
  "Gets a memory address from the tape depending on relative-base, operating mode, and ptr"
  [tape relative-base mode ptr]
  (case mode
    0
    (read-tape tape ptr)

    2
    (+ (read-tape tape ptr) relative-base)))

(defn- assoc-value
  "Updates an exact address on the tape to a new value."
  [tape addr value]
  (assoc tape addr value))

(defn- take-params
  "Given the tape, instruction ptr, relative base, n, and a lazy seq of the instruction's modes, gets n params in proper mode
  Set write? to true if the instruction does a write and therefore should return an address and not a value for the last param."
  [tape ptr relative-base param-modes n write?]
  (->> (take n param-modes)
       (map-indexed #(if (and write? (= (dec n) %1))
                       (get-addr tape relative-base %2 (+ ptr 1 %1))
                       (get-param-value tape relative-base %2 (+ ptr 1 %1))))))

(defn run
  [instructions in-chan out-chan]
  (async/go-loop [tape (->> instructions (map-indexed vector) (into {}))
                  ptr 0
                  relative-base 0]
    (let [instruction (read-tape tape ptr)
          [opcode param-modes] ((juxt first rest) (instruction->opcode-and-param-modes instruction))
          take-params* (partial take-params tape ptr relative-base param-modes)
          assoc-value* (partial assoc-value tape)
          [next-tape next-ptr next-relative-base]
          (case opcode
            1 (let [[a b write-addr] (take-params* 3 true)]
                [(assoc-value* write-addr (+ a b))
                 (+ ptr 4)
                 relative-base])
            2 (let [[a b write-addr] (take-params* 3 true)]
                [(assoc-value* write-addr (* a b))
                 (+ ptr 4)
                 relative-base])
            3 (let [[write-addr] (take-params* 1 true)
                    value (async/<! in-chan)]
                [(assoc-value* write-addr value)
                 (+ ptr 2)
                 relative-base])
            4 (let [[value] (take-params* 1 false)]
                (async/put! out-chan value)
                [tape
                 (+ ptr 2)
                 relative-base])
            5 (let [[test new-ptr] (take-params* 2 false)]
                [tape
                 (if (not (zero? test))
                   new-ptr
                   (+ ptr 3))
                 relative-base])
            6 (let [[test new-ptr] (take-params* 2 false)]
                [tape
                 (if (zero? test)
                   new-ptr
                   (+ ptr 3))
                 relative-base])
            7 (let [[a b write-addr] (take-params* 3 true)]
                [(assoc-value* write-addr (if (< a b) 1 0))
                 (+ ptr 4)
                 relative-base])
            8 (let [[a b write-addr] (take-params* 3 true)]
                [(assoc-value* write-addr (if (= a b) 1 0))
                 (+ ptr 4)
                 relative-base])
            9 (let [[a] (take-params* 1 false)]
                [tape
                 (+ ptr 2)
                 (+ relative-base a)])
            99 nil)]
      (when (some? next-tape)
        (recur next-tape next-ptr next-relative-base)))))

(comment
  "Day 7 Part 2 example w/ my puzzle input"
  (time
    (let [program [3,8,1001,8,10,8,105,1,0,0,21,30,47,60,81,102,183,264,345,426,99999,3,9,1002,9,5,9,4,9,99,3,9,1002,9,5,9,1001,9,4,9,1002,9,4,9,4,9,99,3,9,101,2,9,9,1002,9,4,9,4,9,99,3,9,1001,9,3,9,1002,9,2,9,101,5,9,9,1002,9,2,9,4,9,99,3,9,102,4,9,9,101,4,9,9,1002,9,3,9,101,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,99]]
      (->> (range 5 10)
           comb/permutations
           (pmap (fn [[a b c d e]]
                   (let [ea-chan (async/chan)
                         ab-chan (async/chan)
                         bc-chan (async/chan)
                         cd-chan (async/chan)
                         de-chan (async/chan)
                         a-done (run program ea-chan ab-chan)
                         b-done (run program ab-chan bc-chan)
                         c-done (run program bc-chan cd-chan)
                         d-done (run program cd-chan de-chan)
                         e-done (run program de-chan ea-chan)]
                     (async/put! ea-chan a)
                     (async/put! ab-chan b)
                     (async/put! bc-chan c)
                     (async/put! cd-chan d)
                     (async/put! de-chan e)
                     (async/put! ea-chan 0)
                     (async/<!! e-done)
                     (async/<!! ea-chan))))
           (apply max)))))









