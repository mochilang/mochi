(ns main)

(defrecord _Group [key Items])

(defn _group_by [src keyfn]
  (let [groups (atom {})
        order (atom [])]
    (doseq [it src]
      (let [k (keyfn it)
            ks (str k)]
        (when-not (contains? @groups ks)
          (swap! groups assoc ks (_Group. k []))
          (swap! order conj ks))
        (swap! groups update ks (fn [g] (assoc g :Items (conj (:Items g) it)))))
    )
    (map (fn [k] (@groups k)) @order)))

(defn _query [src joins opts]
  (let [items (atom (mapv vector src))]
    (doseq [j joins]
      (let [joined (atom [])]
        (cond
          (and (:right j) (:left j))
            (let [matched (boolean-array (count (:items j)))]
              (doseq [left @items]
                (let [m (atom false)]
                  (doseq [[ri right] (map-indexed vector (:items j))]
                    (let [keep (if-let [f (:on j)]
                                 (apply f (conj left right))
                                 true)]
                      (when keep
                        (reset! m true)
                        (aset matched ri true)
                        (swap! joined conj (conj left right))))
                  (when-not @m
                    (swap! joined conj (conj left nil))))
              (doseq [[ri right] (map-indexed vector (:items j))]
                (when-not (aget matched ri)
                  (swap! joined conj (vec (concat (repeat (count (first (or @items []))) nil) [right])))))
            (reset! items @joined)
          (:right j)
            (do
              (doseq [right (:items j)]
                (let [m (atom false)]
                  (doseq [left @items]
                    (let [keep (if-let [f (:on j)]
                                 (apply f (conj left right))
                                 true)]
                      (when keep
                        (reset! m true)
                        (swap! joined conj (conj left right))))
                  (when-not @m
                    (swap! joined conj (vec (concat (repeat (count (first (or @items []))) nil) [right])))))
              (reset! items @joined))
          :else
            (do
              (doseq [left @items]
                (let [m (atom false)]
                  (doseq [right (:items j)]
                    (let [keep (if-let [f (:on j)]
                                 (apply f (conj left right))
                                 true)]
                      (when keep
                        (reset! m true)
                        (swap! joined conj (conj left right))))
                  (when (and (:left j) (not @m))
                    (swap! joined conj (conj left nil))))
              (reset! items @joined))))
    (let [it @items
          it (if-let [w (:where opts)] (vec (filter #(apply w %) it)) it)
          it (if-let [sk (:sortKey opts)] (vec (sort-by #(apply sk %) it)) it)
          it (if (contains? opts :skip) (vec (drop (:skip opts) it)) it)
          it (if (contains? opts :take) (vec (take (:take opts) it)) it)]
      (mapv #(apply (:select opts) %) it)))
(declare data groups tmp result)

(defn -main []
  (def data [{:tag "a" :val 1} {:tag "a" :val 2} {:tag "b" :val 3}]) ;; list of map of string to any
  (def groups (let [_src data
      _rows (_query _src [

      ] { :select (fn [d] [d]) })
      _groups (_group_by _rows (fn [d] (:tag d)))
  (vec (map (fn [g] g) _groups)))) ;; list of any
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
(def result (vec (->> (for [r tmp] r) (sort-by (fn [r] (:tag r)))))) ;; list of any
(println result)
)

(-main)
