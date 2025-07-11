(ns main)

(defrecord _Group [key Items])

(defn _group_by [src keyfn]
  (let [groups (transient {})
        order (transient [])]
    (doseq [it src]
      (let [k (keyfn it)
            ks (str k)
            g (get groups ks)]
        (if g
          (assoc! groups ks (assoc g :Items (conj (:Items g) it)))
          (do
            (assoc! groups ks (_Group. k [it]))
            (conj! order ks))))
    (let [g (persistent! groups)
          o (persistent! order)]
      (mapv #(get g %) o))) )

(defn _sort_key [k]
  (cond
    (map? k) (pr-str (into (sorted-map) k))
    (sequential? k) (vec k)
    :else k))
(declare data groups tmp result)

(defn -main []
  (def data [{:tag "a" :val 1} {:tag "a" :val 2} {:tag "b" :val 3}]) ;; list of 
  (def groups (map (fn [g] g) (_group_by data (fn [d] (:tag d))))) ;; list of any
  (def tmp []) ;; list of any
  (loop [_tmp0 (seq groups)]
    (when _tmp0
      (let [g (clojure.core/first _tmp0)]
        (let [r (try
          (def total 0) ;; int
          (loop [_tmp1 (seq (:items g))]
            (when _tmp1
              (let [x (clojure.core/first _tmp1)]
                (let [r (try
                  (def total (+ total (:val x))) ;; any
                  :next
                (catch clojure.lang.ExceptionInfo e
                  (cond
                    (= (.getMessage e) "continue") :next
                    (= (.getMessage e) "break") :break
                    :else (throw e))
                  )
                )]
              (cond
                (= r :break) nil
                :else (recur (next _tmp1))
              )
            )
          )
        )
      )
      (def tmp (conj tmp {:tag (:key g) :total total})) ;; list of any
      :next
    (catch clojure.lang.ExceptionInfo e
      (cond
        (= (.getMessage e) "continue") :next
        (= (.getMessage e) "break") :break
        :else (throw e))
      )
    )]
  (cond
    (= r :break) nil
    :else (recur (next _tmp0))
  )
)
)
)
)
(def result (vec (->> (for [r tmp] r) (sort-by (fn [r] (_sort_key (:tag r))))))) ;; list of any
(println result)
)

(-main)
)
