(ns main)

(require 'clojure.set)

(defn json-str [x]
  (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json-str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json-str x)) "]") (string? x) (pr-str x) :else (str x)))

(defrecord Anon1 [name age])

(def people [{:name "Alice" :age 30} {:name "Bob" :age 25}])

(defn -main []
  (doseq [item people] (println (json-str item))))

(-main)
