(ns main
  (:require
    [clojure.data.json :as json]
  ))

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

(defn _save [rows path opts]
  (let [fmt (get opts :format "csv")
        header (get opts :header false)
        delim (first (or (get opts :delimiter ",") ","))
        headers (if (seq rows) (sort (keys (first rows))) [])]
    (cond
      (= fmt "csv")
        (let [lines (concat
                      (when header [(clojure.string/join delim headers)])
                      (map (fn [r]
                             (clojure.string/join delim (map #(str (get r % "")) headers)))
                           rows))
              out (str (clojure.string/join "\n" lines) "\n")]
          (if (or (nil? path) (= path "") (= path "-"))
            (print out)
            (spit path out)))
      (= fmt "tsv")
        (_save rows path (assoc opts :format "csv" :delimiter "\t"))
      (= fmt "json")
        (let [out (_to_json rows)]
          (if (or (nil? path) (= path "") (= path "-"))
            (print out)
            (spit path out)))
      (= fmt "jsonl")
        (let [out (clojure.string/join "\n" (map _to_json rows))]
          (if (or (nil? path) (= path "") (= path "-"))
            (print (str out "\n"))
            (spit path (str out "\n"))))
      :else
        nil)) )

(declare people)

(defn -main []
  (def people [{:name "Alice" :age 30} {:name "Bob" :age 25}]) ;; list of 
  (_save people "-" {:format "jsonl"})
)

(-main)
