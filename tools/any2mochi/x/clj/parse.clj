(ns any2mochi.parse
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]))

(defn node [expr]
  (if (sequential? expr)
    {:list (mapv node expr)}
    {:atom (str expr)}))

(defn param-list [p]
  (mapv str p))

(defn build-form [expr]
  (if (and (sequential? expr) (seq expr))
    (let [[head & rest] expr]
      (cond
        (= head 'defn)
        (let [[name params & body] rest]
          {:type "defn" :name (str name) :params (param-list params) :body (mapv node body)})
        (= head 'def)
        (let [[name value] rest]
          {:type "def" :name (str name) :value (node value)})
        :else {:type "expr" :body [(node expr)]}))
    {:type "expr" :body [(node expr)]}))

(defn parse-file [f]
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. f))]
    (binding [*read-eval* false]
      (loop [forms []]
        (let [form (read r nil nil)]
          (if (nil? form)
            {:forms forms}
            (recur (conj forms (build-form form))))))))

(defn -main [& [file]]
  (-> (parse-file file)
      (json/write-str)
      (println)))
