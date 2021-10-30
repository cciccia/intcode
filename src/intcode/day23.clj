(ns intcode.day23
  (:require [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as impl]
            [intcode.processor :as intcode]
            [intcode.utils :as utils])
  (:import (clojure.lang Counted)
           (java.util LinkedList)))

(deftype DefaultBuffer [^LinkedList buf ^long n default-value]
  impl/Buffer
  (full? [_this]
    (>= (.size buf) n))
  (remove! [_this]
    (if-not (zero? (.size buf))
      (.removeLast buf)
      default-value))
  (add!* [this itm]
    (when-not (>= (.size buf) n)
      (.addFirst buf itm))
    this)
  (close-buf! [_this])
  Counted
  (count [_this]
    (max 1 (.size buf))))

(defn default-buffer [n default-value]
  (DefaultBuffer. (LinkedList.) n default-value))

(defn non-blocking-chan
  "Uses the above buffer such that any attempts to pull off this channel
  when empty will receive default-value instead of blocking, otherwise works like normal"
  [default-value]
  (async/chan (default-buffer 1024 default-value)))

(defn nat-sniff!
  "if nat is in use, this is the 'timeout' loop that polls te nat-sniff-chan,
   which is a channel that detects any network activity.
   If none has happened for 100ms, which seems to be long enough for purposes of this project,
   sends the last packet it received on node 255 got to node 0"
  [system nat-sniff-chan]
  (async/go-loop [last-sent-idle-y nil]
    (let [[_val chan] (async/alts! [nat-sniff-chan (async/timeout 100)])]
      (if (not= chan nat-sniff-chan)
        (let [idle-x (async/<! (get-in system [255 :stdin]))
              idle-y (async/<! (get-in system [255 :stdin]))]
          (if (= idle-y last-sent-idle-y)
            (do (println "It's this: " idle-y)
                (async/put! (get-in system [255 :stdout]) idle-y))
            (do (println "Sending idle packet to 0:" [idle-x idle-y])
                (async/put! (get-in system [0 :stdin]) idle-x)
                (async/put! (get-in system [0 :stdin]) idle-y)
                (recur idle-y))))
        (do
          (println "Got this: " _val)
          (recur last-sent-idle-y))))))

(defn system!
  "Sets up maps of nodes to their input output channels,
  then links up senders to receivers, and then starts processing packets.
  If nat is in play, also alerts it to when the network is not idle"
  [program nat?]
  (let [system (-> (reduce
                     (fn [acc i]
                       (assoc acc i {:stdin  (non-blocking-chan -1)
                                     :stdout (async/chan)}))
                     {}
                     (range 50))
                   (assoc 255 {:stdin  (async/chan (async/sliding-buffer 2))
                               :stdout (async/chan)}))
        nat-sniff-chan (when nat? (async/chan))]
    (when nat-sniff-chan (nat-sniff! system nat-sniff-chan))
    (doseq [[i {:keys [stdin stdout]}] system]
      (when (< i 255)
        (async/put! stdin i)
        (async/go-loop []
          (let [address    (async/<! stdout)
                x          (async/<! stdout)
                y          (async/<! stdout)
                dest-stdin (get-in system [address :stdin])]
            (println (format "I am: %d Routing packet to: %d: %d %d" i address x y))
            (async/put! dest-stdin x)
            (async/put! dest-stdin y)
            (when (some? nat-sniff-chan)
              (async/put! nat-sniff-chan x)
              (async/put! nat-sniff-chan y))
            (when (some? y)
              (recur))))
        (intcode/run program stdin stdout -1)))
    system))

(defn part1
  [program]
  (let [system (system! program false)
        _x     (async/<!! (get-in system [255 :stdin]))
        y      (async/<!! (get-in system [255 :stdin]))]
    y))

(defn part2
  [program]
  (let [system (system! program true)]
    (async/<!! (get-in system [255 :stdout]))))

(comment
  (part1 (utils/load-edn-input "23.edn")))

(comment
  (part2 (utils/load-edn-input "23.edn")))


(comment
  (let [c (async/chan (default-buffer 1024 -1))]
    (async/<!! c)))

(comment
  (let [c (non-blocking-chan)]
    (async/put! c 50)
    (async/put! c 100)
    [(async/<!! c) (async/<!! c) (async/<!! c)]))

