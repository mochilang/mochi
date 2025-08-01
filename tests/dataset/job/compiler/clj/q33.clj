; Generated by Mochi compiler v0.10.25 on 2025-07-13T12:23:21Z
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

(declare company_name info_type kind_type link_type movie_companies movie_info_idx movie_link title rows result)

(defn test_Q33_finds_linked_TV_series_with_low_rated_sequel []
  (assert (_equal result [{:first_company "US Studio" :second_company "GB Studio" :first_rating "7.0" :second_rating "2.5" :first_movie "Series A" :second_movie "Series B"}]) "expect failed")
)

(defn -main []
  (def company_name [{:id 1 :name "US Studio" :country_code "[us]"} {:id 2 :name "GB Studio" :country_code "[gb]"}]) ;; list of
  (def info_type [{:id 1 :info "rating"} {:id 2 :info "other"}]) ;; list of
  (def kind_type [{:id 1 :kind "tv series"} {:id 2 :kind "movie"}]) ;; list of
  (def link_type [{:id 1 :link "follows"} {:id 2 :link "remake of"}]) ;; list of
  (def movie_companies [{:movie_id 10 :company_id 1} {:movie_id 20 :company_id 2}]) ;; list of
  (def movie_info_idx [{:movie_id 10 :info_type_id 1 :info "7.0"} {:movie_id 20 :info_type_id 1 :info "2.5"}]) ;; list of
  (def movie_link [{:movie_id 10 :linked_movie_id 20 :link_type_id 1}]) ;; list of
  (def title [{:id 10 :title "Series A" :kind_id 1 :production_year 2004} {:id 20 :title "Series B" :kind_id 1 :production_year 2006}]) ;; list of
  (def rows (vec (->> (for [cn1 company_name mc1 movie_companies :when (_equal (:id cn1) (:company_id mc1)) t1 title :when (_equal (:id t1) (:movie_id mc1)) mi_idx1 movie_info_idx :when (_equal (:movie_id mi_idx1) (:id t1)) it1 info_type :when (_equal (:id it1) (:info_type_id mi_idx1)) kt1 kind_type :when (_equal (:id kt1) (:kind_id t1)) ml movie_link :when (_equal (:movie_id ml) (:id t1)) t2 title :when (_equal (:id t2) (:linked_movie_id ml)) mi_idx2 movie_info_idx :when (_equal (:movie_id mi_idx2) (:id t2)) it2 info_type :when (_equal (:id it2) (:info_type_id mi_idx2)) kt2 kind_type :when (_equal (:id kt2) (:kind_id t2)) mc2 movie_companies :when (_equal (:movie_id mc2) (:id t2)) cn2 company_name :when (_equal (:id cn2) (:company_id mc2)) lt link_type :when (_equal (:id lt) (:link_type_id ml)) :when (and (and (and (and (and (and (and (and (_equal (:country_code cn1) "[us]") (_equal (:info it1) "rating")) (_equal (:info it2) "rating")) (_equal (:kind kt1) "tv series")) (_equal (:kind kt2) "tv series")) (or (or (_equal (:link lt) "sequel") (_equal (:link lt) "follows")) (_equal (:link lt) "followed by"))) (< (compare (:info mi_idx2) "3.0") 0)) (>= (:production_year t2) 2005)) (<= (:production_year t2) 2008))] {:first_company (:name cn1) :second_company (:name cn2) :first_rating (:info mi_idx1) :second_rating (:info mi_idx2) :first_movie (:title t1) :second_movie (:title t2)})))) ;; list of
  (def result [{:first_company (_min (vec (->> (for [r rows] (:first_company r))))) :second_company (_min (vec (->> (for [r rows] (:second_company r))))) :first_rating (_min (vec (->> (for [r rows] (:first_rating r))))) :second_rating (_min (vec (->> (for [r rows] (:second_rating r))))) :first_movie (_min (vec (->> (for [r rows] (:first_movie r))))) :second_movie (_min (vec (->> (for [r rows] (:second_movie r)))))}]) ;; list of
  (_json result)
  (test_Q33_finds_linked_TV_series_with_low_rated_sequel)
)

(-main)
