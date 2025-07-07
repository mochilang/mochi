(ns any2mochi.parse
  (:require [clojure.data.json :as json]
            [clojure.tools.reader.edn :as edn]
            [clojure.tools.reader.reader-types :as rt]))

(defn node [expr]
  (let [m (meta expr)]
    (cond
      (sequential? expr)
      {:list (mapv node expr)
       :line (:line m)
       :col (:column m)
       :end-line (:end-line m)
       :end-col (:end-column m)}
      :else
      {:atom (str expr)
       :line (:line m)
       :col (:column m)
       :end-line (:end-line m)
       :end-col (:end-column m)})))

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
            {:type "defn" :name (str name) :params (param-list params) :doc doc
             :body (mapv node body)
             :line (:line m) :col (:column m)
             :end-line (:end-line m) :end-col (:end-column m)})
          (= head 'def)
          (let [[name value] rest]
            {:type "def" :name (str name) :value (node value)
             :line (:line m) :col (:column m)
             :end-line (:end-line m) :end-col (:end-column m)})
          :else {:type "expr" :body [(node expr)]
                 :line (:line m) :col (:column m)
                 :end-line (:end-line m) :end-col (:end-column m)}))
      {:type "expr" :body [(node expr)]
       :line (:line m) :col (:column m)
       :end-line (:end-line m) :end-col (:end-column m)}))

(defn parse-file [f]
  (with-open [r (rt/indexing-push-back-reader (java.io.FileReader. f))]
    (binding [*read-eval* false]
      (loop [forms []]
        (let [form (edn/read {:eof nil :read-cond :allow :features #{:clj}
                               :track-position? true} r)]
          (if (nil? form)
            {:forms forms}
            (recur (conj forms (build-form form))))))))

(defn -main [& [file]]
  (-> (parse-file file)
      (json/write-str)
      (println)))
