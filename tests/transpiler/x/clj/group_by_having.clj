(ns main)

(require 'clojure.set)

(defrecord People [name city])

(def people [{:name "Alice" :city "Paris"} {:name "Bob" :city "Hanoi"} {:name "Charlie" :city "Paris"} {:name "Diana" :city "Hanoi"} {:name "Eve" :city "Paris"} {:name "Frank" :city "Hanoi"} {:name "George" :city "Paris"}])

(def big (for [g (for [[k rows] (group-by :key (for [p people :let [k (:city p)]] {:key k :item p})) :let [g {:key k :items (map :item rows)}] :when (>= (count (:items g)) 4)] g)] {:city (:key g) :num (count (:items g))}))

(defn -main []
  (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) big)))

(-main)
