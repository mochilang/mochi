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

(declare keyword movie_info movie_keyword title allowed_infos candidate_titles result)

(defn test_Q3_returns_lexicographically_smallest_sequel_title []
  (assert (_equal result [{:movie_title "Alpha"}]) "expect failed")
)

(defn -main []
  (def keyword [{:id 1 :keyword "amazing sequel"} {:id 2 :keyword "prequel"}]) ;; list of
  (def movie_info [{:movie_id 10 :info "Germany"} {:movie_id 30 :info "Sweden"} {:movie_id 20 :info "France"}]) ;; list of
  (def movie_keyword [{:movie_id 10 :keyword_id 1} {:movie_id 30 :keyword_id 1} {:movie_id 20 :keyword_id 1} {:movie_id 10 :keyword_id 2}]) ;; list of
  (def title [{:id 10 :title "Alpha" :production_year 2006} {:id 30 :title "Beta" :production_year 2008} {:id 20 :title "Gamma" :production_year 2009}]) ;; list of
  (def allowed_infos ["Sweden" "Norway" "Germany" "Denmark" "Swedish" "Denish" "Norwegian" "German"]) ;; list of string
  (def candidate_titles (vec (->> (for [k keyword mk movie_keyword :when (_equal (:keyword_id mk) (:id k)) mi movie_info :when (_equal (:movie_id mi) (:movie_id mk)) t title :when (_equal (:id t) (:movie_id mi)) :when (and (and (and (clojure.string/includes? (:keyword k) "sequel") (some #(= (:info mi) %) allowed_infos)) (> (:production_year t) 2005)) (_equal (:movie_id mk) (:movie_id mi)))] (:title t))))) ;; list of string
  (def result [{:movie_title (_min candidate_titles)}]) ;; list of
  (_json result)
  (test_Q3_returns_lexicographically_smallest_sequel_title)
)

(-main)
