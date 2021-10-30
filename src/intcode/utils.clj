(ns intcode.utils
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import (java.io PushbackReader)))

(defn load-edn-input
  [resource-file]
  (edn/read (PushbackReader. (io/reader (io/resource resource-file)))))

(defn converge-via
  [f x]
  (reductions
    (fn [x x']
      (if (= x x')
        (reduced x')
        x'))
    (iterate f x)))