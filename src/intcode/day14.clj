(ns intcode.day14
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn ordered-by-dependencies
  [elements dependencies]
  (loop [ordered []
         remaining elements]
    (let [next-elements (set (filter
                               (fn [element]
                                 (every? #(not (and (set/subset? (get dependencies element) (get dependencies %))
                                                    (not= (get dependencies element) (get dependencies %))))
                                         (disj remaining element)))
                               remaining))]
      (if (empty? next-elements)
        (concat ordered remaining)
        (let [next-remaining (set/difference remaining next-elements)]
          (if (pos? (count next-remaining))
            (recur (into ordered next-elements) next-remaining)
            (into ordered next-elements)))))))

(defn get-dependencies
  [formulae]
  (letfn [(deps [element] (let [reagents (->> (get formulae element)
                                              second
                                              (map first)
                                              set)]
                            (apply set/union (concat [reagents] (map deps reagents)))))]

    (reduce
      (fn [acc [k _]]
        (assoc acc k (deps k)))
      {}
      formulae)))

(defn get-formulae
  [filename]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource filename))]
    (let [formulae (reduce
                     (fn [acc line]
                       (let [[reagents result] (str/split line #" => ")
                             [result-amt result-elem] (str/split result #" ")
                             reagent-vec (->> (str/split reagents #", ")
                                              (mapv (fn [reagent] (-> (str/split reagent #" ")
                                                                      reverse
                                                                      vec
                                                                      (update 1 #(Long/parseLong %))))))]
                         (assoc acc result-elem [(Long/parseLong result-amt) reagent-vec])))
                     {}
                     (line-seq rdr))]
      formulae)))

(defn break-down
  [formulae ordered-elems mats]
  (let [[elem needed-amt] (some #(when (contains? mats %) [% (get mats %)]) ordered-elems)
        [conversion-amt new-mats] (get formulae elem)
        multiplier (long (Math/ceil (/ needed-amt conversion-amt)))]
    (-> (reduce
          (fn [acc [new-e new-amt]] (update acc new-e #(+ (or % 0) (* new-amt multiplier))))
          mats
          new-mats)
        (dissoc elem))))


(defn part-1
  [filename fuel-to-make]
  (let [formulae (get-formulae filename)
        deps (get-dependencies formulae)
        elems (set (map first deps))
        ordered-elems (ordered-by-dependencies elems deps)]
    (loop [current-mats {"FUEL" fuel-to-make}]
      (if (= ["ORE" 1]
             ((juxt first count) (keys current-mats)))
        (get current-mats "ORE")
        (recur (break-down formulae ordered-elems current-mats))))))

(defn part-2
  [filename fuel-to-make]
  (part-1 filename fuel-to-make))



(comment
  (part-2 "14.txt" 4436981))


