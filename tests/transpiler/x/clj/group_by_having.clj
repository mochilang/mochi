(ns main)

(require 'clojure.set)

(defn json-str [x]
  (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json-str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json-str x)) "]") (string? x) (pr-str x) :else (str x)))

(defrecord Anon1 [name city])

(def people [{:name "Alice" :city "Paris"} {:name "Bob" :city "Hanoi"} {:name "Charlie" :city "Paris"} {:name "Diana" :city "Hanoi"} {:name "Eve" :city "Paris"} {:name "Frank" :city "Hanoi"} {:name "George" :city "Paris"}])

(def big (for [g (for [[k rows] (group-by :key (for [p people :let [k (:city p)]] {:key k :item p})) :let [g {:key k :items (map :item rows)}] :when (>= (count (:items g)) 4)] g)] {:city (:key g) :num (count (:items g))}))

(defn -main []
  (println (json-str big)))

(-main)
