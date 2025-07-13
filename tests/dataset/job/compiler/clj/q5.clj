(ns main)

(defn _min [v]
  (let [lst (cond
              (and (map? v) (contains? v :Items)) (:Items v)
              (sequential? v) v
              :else (throw (ex-info "min() expects list or group" {})))]
    (if (empty? lst)
      0
      (reduce (fn [a b] (if (neg? (compare a b)) a b)) lst))))

(defn _equal [a b]
  (cond
    (and (sequential? a) (sequential? b))
      (and (= (count a) (count b)) (every? true? (map _equal a b)))
    (and (map? a) (map? b))
      (and (= (count a) (count b))
           (every? (fn [k] (_equal (get a k) (get b k))) (keys a)))
    (and (number? a) (number? b))
      (= (double a) (double b))
    :else
      (= a b)))

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

(defn _json [v]
  (println (_to_json v)))

(declare company_type info_type title movie_companies movie_info candidate_titles result)

(defn test_Q5_finds_the_lexicographically_first_qualifying_title []
  (assert (_equal result [{:typical_european_movie "A Film"}]) "expect failed")
)

(defn -main []
  (def company_type [{:ct_id 1 :kind "production companies"} {:ct_id 2 :kind "other"}]) ;; list of
  (def info_type [{:it_id 10 :info "languages"}]) ;; list of
  (def title [{:t_id 100 :title "B Movie" :production_year 2010} {:t_id 200 :title "A Film" :production_year 2012} {:t_id 300 :title "Old Movie" :production_year 2000}]) ;; list of
  (def movie_companies [{:movie_id 100 :company_type_id 1 :note "ACME (France) (theatrical)"} {:movie_id 200 :company_type_id 1 :note "ACME (France) (theatrical)"} {:movie_id 300 :company_type_id 1 :note "ACME (France) (theatrical)"}]) ;; list of
  (def movie_info [{:movie_id 100 :info "German" :info_type_id 10} {:movie_id 200 :info "Swedish" :info_type_id 10} {:movie_id 300 :info "German" :info_type_id 10}]) ;; list of
  (def candidate_titles (vec (->> (for [ct company_type mc movie_companies :when (_equal (:company_type_id mc) (:ct_id ct)) mi movie_info :when (_equal (:movie_id mi) (:movie_id mc)) it info_type :when (_equal (:it_id it) (:info_type_id mi)) t title :when (_equal (:t_id t) (:movie_id mc)) :when (and (and (and (and (_equal (:kind ct) "production companies") (clojure.string/includes? (:note mc) "(theatrical)")) (clojure.string/includes? (:note mc) "(France)")) (> (:production_year t) 2005)) (some #(= (:info mi) %) ["Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German"]))] (:title t))))) ;; list of string
  (def result [{:typical_european_movie (_min candidate_titles)}]) ;; list of
  (_json result)
  (test_Q5_finds_the_lexicographically_first_qualifying_title)
)

(-main)
