(ns main)

(require 'clojure.set)

(defrecord Anon1 [a b])

(def m {:a 1 :b 2})

(defn -main []
  (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) m)))

(-main)
