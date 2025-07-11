(ns main)

(defn _parse_csv [text header delim]
  (let [lines (->> (clojure.string/split-lines text)
                   (remove clojure.string/blank?))
        headers (if header
                    (clojure.string/split (first lines) (re-pattern (str delim)))
                    (map #(str "c" %) (range (count (clojure.string/split (first lines) (re-pattern (str delim)))))))]
    (mapv (fn [line]
            (let [parts (clojure.string/split line (re-pattern (str delim)))]
              (zipmap headers parts)))
          (drop (if header 1 0) lines))) )

(defn _load [path opts]
  (let [fmt (get opts :format "csv")
        header (get opts :header true)
        delim (first (or (get opts :delimiter ",") ","))
        text (if (or (nil? path) (= path "") (= path "-"))
               (slurp *in*)
               (slurp path))]
    (cond
      (= fmt "csv") (_parse_csv text header delim)
      (= fmt "tsv") (_parse_csv text header "\t")
      (= fmt "json")
        (let [data (clojure.edn/read-string text)]
          (cond
            (map? data) [data]
            (sequential? data) (vec data)
            :else []))
      (= fmt "jsonl")
        (->> (clojure.string/split-lines text)
             (remove clojure.string/blank?)
             (mapv clojure.edn/read-string))
      (= fmt "yaml")
        (->> (clojure.string/split text #"(?m)^- ")
             (map clojure.string/trim)
             (remove clojure.string/blank?)
             (mapv (fn [blk]
                     (->> (clojure.string/split-lines blk)
                          (map clojure.string/trim)
                          (remove clojure.string/blank?)
                          (map (fn [line]
                                 (let [[k v] (clojure.string/split line #":\s*" 2)]
                                   [(keyword k) v])))
                          (into {})))))
      :else [])) )

(declare people adults)

(defn Person [name age email]
  {:__name "Person" :name name :age age :email email}
)


(defn -main []
  (def people (mapv Person (_load "../interpreter/valid/people.yaml" {:format "yaml"}))) ;; list of Person
  (def adults (vec (->> (for [p people :when (>= (:age p) 18)] {:name (:name p) :email (:email p)})))) ;; list of 
  (loop [_tmp0 (seq adults)]
    (when _tmp0
      (let [a (clojure.core/first _tmp0)]
        (let [r (try
          (println (:name a) (:email a))
          :next
        (catch clojure.lang.ExceptionInfo e
          (cond
            (= (.getMessage e) "continue") :next
            (= (.getMessage e) "break") :break
            :else (throw e))
          )
        )]
      (cond
        (= r :break) nil
        :else (recur (next _tmp0))
      )
    )
  )
)
)
)

(-main)
