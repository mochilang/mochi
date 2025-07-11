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

(defn _escape_json [s]
  (-> s
      (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")))

(defn _to_json [v]
  (cond
    (nil? v) "null"
    (string? v) (str "\"" (_escape_json v) "\"")
    (number? v) (str v)
    (boolean? v) (str v)
    (sequential? v) (str "[" (clojure.string/join "," (map _to_json v)) "]")
    (map? v) (str "{" (clojure.string/join "," (map (fn [[k val]]
                                        (str "\"" (_escape_json (name k)) "\":" (_to_json val))) v)) "}")
    :else (str "\"" (_escape_json (str v)) "\"")))

(defn _json [v]
  (println (_to_json v)))

(declare people big)

(defn -main []
  (def people [{:name "Alice" :city "Paris"} {:name "Bob" :city "Hanoi"} {:name "Charlie" :city "Paris"} {:name "Diana" :city "Hanoi"} {:name "Eve" :city "Paris"} {:name "Frank" :city "Hanoi"} {:name "George" :city "Paris"}]) ;; list of 
  (def big (map (fn [g] {:city (:key g) :num (count (:Items g))}) (_group_by people (fn [p] (:city p))))) ;; list of 
  (_json big)
)

(-main)
)
