(ns intcode.day18
  (:require [clojure.set :as set]))

(def NORTH 1)
(def SOUTH 2)
(def WEST 3)
(def EAST 4)

(defn move
  [[x y] dir]
  (cond
    (= dir NORTH)
    [x (dec y)]

    (= dir SOUTH)
    [x (inc y)]

    (= dir WEST)
    [(dec x) y]

    (= dir EAST)
    [(inc x) y]))

(defn distances*
  [{:keys [paths origin locks-by-loc keys-by-key keys-by-loc]}]
  (loop [queue [[origin 0 #{}]]
         visited #{}
         keys-in-range {}]
    (if (empty? queue)
      keys-in-range
      (let [[loc distance keys-required] (first queue)]
        (cond
          (contains? visited loc)
          (recur (rest queue)
                 visited
                 keys-in-range)

          (not (contains? paths loc))
          (recur (rest queue)
                 (conj visited loc)
                 keys-in-range)

          (and (contains? keys-by-loc loc)
               (not= loc origin))
          (let [new-keys-required (conj keys-required loc)]
            (recur (concat (rest queue)
                           [[(move loc NORTH) (inc distance) new-keys-required]]
                           [[(move loc SOUTH) (inc distance) new-keys-required]]
                           [[(move loc WEST) (inc distance) new-keys-required]]
                           [[(move loc EAST) (inc distance) new-keys-required]])
                   (conj visited loc)
                   (assoc keys-in-range loc [distance keys-required])))

          (contains? locks-by-loc loc)
          (let [key-loc (get keys-by-key (char (+ 32 (int (get locks-by-loc loc)))))
                keys-required (if key-loc (conj keys-required key-loc) keys-required)]
            (recur (concat (rest queue)
                           [[(move loc NORTH) (inc distance) keys-required]]
                           [[(move loc SOUTH) (inc distance) keys-required]]
                           [[(move loc WEST) (inc distance) keys-required]]
                           [[(move loc EAST) (inc distance) keys-required]])
                   (conj visited loc)
                   keys-in-range))

          :else
          (recur (concat (rest queue)
                         [[(move loc NORTH) (inc distance) keys-required]]
                         [[(move loc SOUTH) (inc distance) keys-required]]
                         [[(move loc WEST) (inc distance) keys-required]]
                         [[(move loc EAST) (inc distance) keys-required]])
                 (conj visited loc)
                 keys-in-range))))))

(def distances (memoize distances*))

(defn build-grid
  [filename]
  (with-open [rdr (clojure.java.io/reader (clojure.java.io/resource filename))]
    (reduce
      (fn [acc [y line]]
        (let [row (reduce
                    (fn [acc [x c]]
                      (cond
                        (<= 65 (int c) 90)
                        (-> (update acc :locks-by-key assoc c [x y])
                            (update :locks-by-loc assoc [x y] c)
                            (update :paths conj [x y]))


                        (<= 97 (int c) 122)
                        (-> (update acc :keys-by-loc assoc [x y] c)
                            (update :keys-by-key assoc c [x y])
                            (update :paths conj [x y]))

                        (= \@ c)
                        (-> (assoc acc :origin [x y])
                            (update :paths conj [x y]))

                        (= \. c)
                        (update acc :paths conj [x y])

                        :else
                        acc))
                    {:keys-by-key  {}
                     :keys-by-loc  {}
                     :locks-by-key {}
                     :locks-by-loc {}
                     :origin       nil
                     :paths        #{}}
                    (map-indexed vector line))]
          (-> acc
              (update :keys-by-key merge (:keys-by-key row))
              (update :keys-by-loc merge (:keys-by-loc row))
              (update :locks-by-key merge (:locks-by-key row))
              (update :locks-by-loc merge (:locks-by-loc row))
              (update :origin #(or % (:origin row)))
              (update :paths set/union (:paths row)))))
      {:keys-by-key  {}
       :keys-by-loc  {}
       :locks-by-key {}
       :locks-by-loc {}
       :origin   nil
       :paths    #{}}
      (map-indexed vector (line-seq rdr)))))

(defn adjust-for-next-key
  [{:keys [keys-by-loc locks paths] :as grid} key-loc]
  (let [lock-loc (get locks (char (- (int (get keys-by-loc key-loc)) 32)))]
    (merge grid {:origin key-loc
                 :paths  (if lock-loc
                           (conj paths lock-loc)
                           paths)})))

(defn all-distances
  [{:keys [keys-by-loc origin] :as grid}]
  (-> (reduce
        #(assoc %1 %2 (distances (adjust-for-next-key grid %2)))
        {}
        (keys keys-by-loc))
      (assoc origin (distances grid))))

(defn part1
  [filename]
  (let [grid (build-grid filename)
        all-distances (all-distances grid)
        distances-from-origin (get all-distances (:origin grid))]
    (loop [total-distances (reduce-kv (fn [acc loc [distance next-required-keys]]
                                        (if (empty? next-required-keys)
                                          (assoc acc [#{loc} loc] distance)
                                          acc))
                                      {}
                                      distances-from-origin)
           queue (apply conj [[#{} (:origin grid)]] (keys total-distances))]
      (if (empty? queue)
        (->> (filter (fn [[collected loc]]
                       (when (= collected (set (keys (:keys-by-loc grid))))
                         (vector collected loc)))
                     (keys total-distances))
             (map #(get total-distances %))
             (apply min))
        (let [[current-collected current-loc] (first queue)
              distances-from-current (get all-distances current-loc)
              next-locs (->> (keys distances-from-current)
                             (filter (fn [next-loc]
                                       (let [[_ next-required-keys] (get distances-from-current next-loc)]
                                         (and (set/subset? next-required-keys current-collected)
                                              (not (contains? current-collected next-loc))))))
                             (mapcat (fn [next-loc]
                                       (let [distances-from-next (get all-distances next-loc)]
                                         (->> (keys distances-from-next)
                                              (filter (fn [next-next-loc]
                                                        (let [[next-next-distance next-next-required-keys] (get distances-from-next next-next-loc)]
                                                          (and (set/subset? next-next-required-keys (conj current-collected next-loc))
                                                               (or (not (contains? total-distances [(conj current-collected next-loc next-next-loc) next-next-loc]))
                                                                   (< (+ next-next-distance (get total-distances [(conj current-collected next-loc) next-loc]))
                                                                      (get total-distances [(conj current-collected next-loc next-next-loc) next-next-loc])))
                                                               (not (contains? current-collected next-next-loc))))))
                                              (mapv (fn [next-next-loc]
                                                      (let [[next-next-distance _] (get distances-from-next next-next-loc)]
                                                        [(conj current-collected next-loc next-next-loc)
                                                         next-next-loc
                                                         (+ next-next-distance (get total-distances [(conj current-collected next-loc) next-loc]))]))))))))]
          (recur (reduce
                   (fn [acc [collected loc distance]]
                     (assoc acc [collected loc] distance))
                   total-distances
                   next-locs)
                 (apply conj (subvec queue 1) (mapv #(vector (first %) (second %)) next-locs))))))))


(comment
  (part1 "18.txt"))

(comment
  (time (part1 "f.txt")))