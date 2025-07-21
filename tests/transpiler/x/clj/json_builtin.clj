(ns main)

(require 'clojure.set)

(defn json-str [x]
  (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json-str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json-str x)) "]") (string? x) (pr-str x) :else (str x)))

(defrecord Anon1 [a b])

(def m {:a 1 :b 2})

(defn -main []
  (println (json-str m)))

(-main)
