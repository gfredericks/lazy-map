(ns lazy-map.core
  (:refer-clojure :exclude [realized?]))

;; Design questions:
;;   do we want a lazy-map? predicate?
;;   would an atom be simpler than two refs? more performant? is it
;;     even doable?
;;   (pprint (clojure.set/difference (-> {} type ancestors) (-> {} lazy-map type ancestors)))
;;
;;     #{java.lang.Runnable java.util.Map clojure.lang.IEditableCollection
;;      clojure.lang.IMeta clojure.lang.AFn clojure.lang.IFn
;;      clojure.lang.APersistentMap clojure.lang.IObj
;;      java.util.concurrent.Callable clojure.lang.MapEquivalence
;;      java.io.Serializable}

(defprotocol ILazyMap
  (contains-yet? [_ k])
  (realized? [_]) ;; is clojure.lang.IPending appropriate for this?
  (tick [_]))

(declare tick-all)

(deftype LazyMap [m entries]
  ILazyMap
  (contains-yet? [_ k]
    (contains? @m k))
  (realized? [_]
    (empty? @entries))
  (tick [_]
    (dosync (let [es @entries]
              (when-not (empty? es)
                (let [[[k v]] es] ;; should we throw an exception if the key has appeared already?
                  (alter m assoc k v)
                  (alter entries rest))))))

  ;; punting on this and IPersistentMap because it is hard or impossible
  clojure.lang.Seqable
  (seq [this]
    (tick-all this)
    (.seq @m))
  
  clojure.lang.IPersistentMap
  (assoc [this k v]
    (tick-all this)
    (.assoc @m k v))
  (assocEx [this k v]
    (tick-all this)
    (.assocEx @m k v))
  (without [this k]
    (tick-all this)
    (.without @m k))

  clojure.lang.ILookup
  (valAt [this k]
    (.valAt this k nil))
  (valAt [this k not-found]
    (loop []
        (let [[done? res] (dosync (cond
                                    (contains? @m k)
                                    [true (@m k)]

                                    (empty? @entries)
                                    [true not-found]

                                    :else
                                    [false nil]))]
          (if done?
            res
            (do (tick this) (recur)))))))

(defn- tick-all
  [lm]
  (when-not (realized? lm)
    (tick lm)
    (recur lm)))

(defn lazy-map
  [entries]
  (LazyMap. (ref {}) (ref (seq entries))))
