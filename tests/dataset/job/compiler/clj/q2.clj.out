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

(declare company_name keyword movie_companies movie_keyword title titles result)

(defn test_Q2_finds_earliest_title_for_German_companies_with_character_keyword []
  (assert (_equal result "Der Film") "expect failed")
)

(defn -main []
  (def company_name [{:id 1 :country_code "[de]"} {:id 2 :country_code "[us]"}]) ;; list of
  (def keyword [{:id 1 :keyword "character-name-in-title"} {:id 2 :keyword "other"}]) ;; list of
  (def movie_companies [{:movie_id 100 :company_id 1} {:movie_id 200 :company_id 2}]) ;; list of
  (def movie_keyword [{:movie_id 100 :keyword_id 1} {:movie_id 200 :keyword_id 2}]) ;; list of
  (def title [{:id 100 :title "Der Film"} {:id 200 :title "Other Movie"}]) ;; list of
  (def titles (vec (->> (for [cn company_name mc movie_companies :when (_equal (:company_id mc) (:id cn)) t title :when (_equal (:movie_id mc) (:id t)) mk movie_keyword :when (_equal (:movie_id mk) (:id t)) k keyword :when (_equal (:keyword_id mk) (:id k)) :when (and (and (_equal (:country_code cn) "[de]") (_equal (:keyword k) "character-name-in-title")) (_equal (:movie_id mc) (:movie_id mk)))] (:title t))))) ;; list of string
  (def result (_min titles)) ;; string
  (_json result)
  (test_Q2_finds_earliest_title_for_German_companies_with_character_keyword)
)

(-main)
