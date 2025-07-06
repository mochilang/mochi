(ns any2mochi.parse
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]))

(defn node [expr]
  (let [m (meta expr)]
    (cond
      (sequential? expr)
      {:list (mapv node expr)
       :line (:line m)
       :col (:column m)}
      :else
      {:atom (str expr)
       :line (:line m)
       :col (:column m)})))

(defn param-list [p]
  (mapv str p))

(defn build-form [expr]
  (let [m (meta expr)]
    (if (and (sequential? expr) (seq expr))
      (let [[head & rest] expr]
        (cond
          (= head 'defn)
          (let [[name params & body] rest
                [doc body] (if (string? (first body))
                             [(first body) (rest body)]
                             [nil body])]
            {:type "defn" :name (str name) :params (param-list params) :doc doc :body (mapv node body) :line (:line m) :col (:column m)})
          (= head 'def)
          (let [[name value] rest]
            {:type "def" :name (str name) :value (node value) :line (:line m) :col (:column m)})
          :else {:type "expr" :body [(node expr)] :line (:line m) :col (:column m)}))
      {:type "expr" :body [(node expr)] :line (:line m) :col (:column m)}))

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
