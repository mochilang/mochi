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
  (def people [{:name "Alice" :city "Paris"} {:name "Bob" :city "Hanoi"} {:name "Charlie" :city "Paris"} {:name "Diana" :city "Hanoi"} {:name "Eve" :city "Paris"} {:name "Frank" :city "Hanoi"} {:name "George" :city "Paris"}]) ;; list of map of string to string
  (def big (map (fn [g] {:city (:key g) :num (count (:Items g))}) (_group_by people (fn [p] (:city p))))) ;; list of map of string to any
  (_json big)
)

(-main)
