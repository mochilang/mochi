(ns main)

(defn _avg [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "avg() expects list or group" {})))]
    (if (empty? lst)
      0
      (/ (reduce + lst) (double (count lst)))))
  )

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

(declare people stats)

(defn -main []
  (def people [{:name "Alice" :age 30 :city "Paris"} {:name "Bob" :age 15 :city "Hanoi"} {:name "Charlie" :age 65 :city "Paris"} {:name "Diana" :age 45 :city "Hanoi"} {:name "Eve" :age 70 :city "Paris"} {:name "Frank" :age 22 :city "Hanoi"}]) ;; list of 
  (def stats (map (fn [g] {:city (:key g) :count (count (:Items g)) :avg_age (_avg (vec (->> (for [p (:Items g)] (:age p)))))}) (_group_by people (fn [person] (:city person))))) ;; list of 
  (println "--- People grouped by city ---")
  (loop [_tmp0 (seq stats)]
    (when _tmp0
      (let [s (clojure.core/first _tmp0)]
        (let [r (try
          (println (:city s) ": count =" (:count s) ", avg_age =" (:avg_age s))
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
)

(-main)
)
