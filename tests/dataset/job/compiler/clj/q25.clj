; Generated by Mochi compiler v0.10.25 on 2025-07-13T12:23:07Z
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

(declare cast_info info_type keyword movie_info movie_info_idx movie_keyword name title allowed_notes allowed_keywords matches result)

(defn test_Q25_finds_male_horror_writer_with_violent_keywords []
  (assert (_equal result [{:movie_budget "Horror" :movie_votes 100 :male_writer "Mike" :violent_movie_title "Scary Movie"}]) "expect failed")
)

(defn -main []
  (def cast_info [{:movie_id 1 :person_id 1 :note "(writer)"} {:movie_id 2 :person_id 2 :note "(writer)"}]) ;; list of
  (def info_type [{:id 1 :info "genres"} {:id 2 :info "votes"}]) ;; list of
  (def keyword [{:id 1 :keyword "murder"} {:id 2 :keyword "romance"}]) ;; list of
  (def movie_info [{:movie_id 1 :info_type_id 1 :info "Horror"} {:movie_id 2 :info_type_id 1 :info "Comedy"}]) ;; list of
  (def movie_info_idx [{:movie_id 1 :info_type_id 2 :info 100} {:movie_id 2 :info_type_id 2 :info 50}]) ;; list of
  (def movie_keyword [{:movie_id 1 :keyword_id 1} {:movie_id 2 :keyword_id 2}]) ;; list of
  (def name [{:id 1 :name "Mike" :gender "m"} {:id 2 :name "Sue" :gender "f"}]) ;; list of
  (def title [{:id 1 :title "Scary Movie"} {:id 2 :title "Funny Movie"}]) ;; list of
  (def allowed_notes ["(writer)" "(head writer)" "(written by)" "(story)" "(story editor)"]) ;; list of string
  (def allowed_keywords ["murder" "blood" "gore" "death" "female-nudity"]) ;; list of string
  (def matches (vec (->> (for [ci cast_info it1 info_type it2 info_type k keyword mi movie_info mi_idx movie_info_idx mk movie_keyword n name t title :when (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (and (some #(= (:note ci) %) allowed_notes) (_equal (:info it1) "genres")) (_equal (:info it2) "votes")) (some #(= (:keyword k) %) allowed_keywords)) (_equal (:info mi) "Horror")) (_equal (:gender n) "m")) (_equal (:id t) (:movie_id mi))) (_equal (:id t) (:movie_id mi_idx))) (_equal (:id t) (:movie_id ci))) (_equal (:id t) (:movie_id mk))) (_equal (:movie_id ci) (:movie_id mi))) (_equal (:movie_id ci) (:movie_id mi_idx))) (_equal (:movie_id ci) (:movie_id mk))) (_equal (:movie_id mi) (:movie_id mi_idx))) (_equal (:movie_id mi) (:movie_id mk))) (_equal (:movie_id mi_idx) (:movie_id mk))) (_equal (:id n) (:person_id ci))) (_equal (:id it1) (:info_type_id mi))) (_equal (:id it2) (:info_type_id mi_idx))) (_equal (:id k) (:keyword_id mk)))] {:budget (:info mi) :votes (:info mi_idx) :writer (:name n) :title (:title t)})))) ;; list of
  (def result [{:movie_budget (_min (vec (->> (for [x matches] (:budget x))))) :movie_votes (apply min (vec (->> (for [x matches] (:votes x))))) :male_writer (_min (vec (->> (for [x matches] (:writer x))))) :violent_movie_title (_min (vec (->> (for [x matches] (:title x)))))}]) ;; list of
  (_json result)
  (test_Q25_finds_male_horror_writer_with_violent_keywords)
)

(-main)
