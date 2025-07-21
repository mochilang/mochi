(ns main)

(require 'clojure.set)

(defrecord Anon1 [name age])

(def people [{:name "Alice" :age 30} {:name "Bob" :age 25}])

(defn -main []
  (doseq [item people] (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) item))))

(-main)
