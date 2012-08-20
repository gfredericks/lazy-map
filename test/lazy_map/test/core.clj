(ns lazy-map.test.core
  (:refer-clojure :exclude [realized?])
  (:use [lazy-map.core])
  (:use [clojure.test]))

(deftest map?-test
  (is (map? (lazy-map {}))))

(deftest basic-test
  (is (= :Bar (get (lazy-map {:foo :Bar}) :foo))))

(deftest infinite-map-test
  (is (= 200 (get (lazy-map (for [x (range)] [x (inc x)])) 199))))

(deftest assoc-test
  (let [m (lazy-map [[2 4] [8 4] [3 1]])
        m2 (assoc m 3 14 9 20)]
    (are [k v] (= v (get m k))
         2 4
         8 4
         3 1)
    (are [k v] (= v (get m2 k))
         2 4
         8 4
         3 14
         9 20)
    (is (= {2 4 8 4 3 1} (into {} m)))
    (is (= {2 4 8 4 3 14 9 20} (into {} m2)))))