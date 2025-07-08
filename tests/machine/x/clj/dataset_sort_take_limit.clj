(ns main)

(declare products expensive)

(defn -main []
  (def products [{:name "Laptop" :price 1500} {:name "Smartphone" :price 900} {:name "Tablet" :price 600} {:name "Monitor" :price 300} {:name "Keyboard" :price 100} {:name "Mouse" :price 50} {:name "Headphones" :price 200}]) ;; list of map of string to any
  (def expensive (vec (->> (for [p products] p) (sort-by (fn [p] (- (:price p)))) (drop 1) (take 3)))) ;; list of map of string to any
  (println "--- Top products (excluding most expensive) ---")
  (loop [_tmp0 (seq expensive)]
    (when _tmp0
      (let [item (clojure.core/first _tmp0)]
        (let [r (try
          (println (:name item) "costs $" (:price item))
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
