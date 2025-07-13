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

(declare company_type info_type title movie_companies movie_info_idx filtered result)

(defn test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production []
  (assert (_equal result {:production_note "ACME (co-production)" :movie_title "Good Movie" :movie_year 1995}) "expect failed")
)

(defn -main []
  (def company_type [{:id 1 :kind "production companies"} {:id 2 :kind "distributors"}]) ;; list of
  (def info_type [{:id 10 :info "top 250 rank"} {:id 20 :info "bottom 10 rank"}]) ;; list of
  (def title [{:id 100 :title "Good Movie" :production_year 1995} {:id 200 :title "Bad Movie" :production_year 2000}]) ;; list of
  (def movie_companies [{:movie_id 100 :company_type_id 1 :note "ACME (co-production)"} {:movie_id 200 :company_type_id 1 :note "MGM (as Metro-Goldwyn-Mayer Pictures)"}]) ;; list of
  (def movie_info_idx [{:movie_id 100 :info_type_id 10} {:movie_id 200 :info_type_id 20}]) ;; list of
  (def filtered (vec (->> (for [ct company_type mc movie_companies :when (_equal (:id ct) (:company_type_id mc)) t title :when (_equal (:id t) (:movie_id mc)) mi movie_info_idx :when (_equal (:movie_id mi) (:id t)) it info_type :when (_equal (:id it) (:info_type_id mi)) :when (and (and (and (_equal (:kind ct) "production companies") (_equal (:info it) "top 250 rank")) (not (clojure.string/includes? (:note mc) "(as Metro-Goldwyn-Mayer Pictures)"))) (or (clojure.string/includes? (:note mc) "(co-production)") (clojure.string/includes? (:note mc) "(presents)")))] {:note (:note mc) :title (:title t) :year (:production_year t)})))) ;; list of
  (def result {:production_note (_min (vec (->> (for [r filtered] (:note r))))) :movie_title (_min (vec (->> (for [r filtered] (:title r))))) :movie_year (apply min (vec (->> (for [r filtered] (:year r)))))}) ;;
  (_json [result])
  (test_Q1_returns_min_note__title_and_year_for_top_ranked_co_production)
)

(-main)
